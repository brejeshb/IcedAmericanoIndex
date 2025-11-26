
import asyncio
import pandas as pd
from pyppeteer import launch
import json
import random
import warnings
import nest_asyncio
from openai import OpenAI
from bs4 import BeautifulSoup
from tqdm import tqdm
from dotenv import load_dotenv
import os

warnings.filterwarnings("ignore")
nest_asyncio.apply()

load_dotenv()
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")
client = OpenAI(api_key=OPENAI_API_KEY)


def clean_html_for_gpt(html_content: str) -> str:
    """Simplifies HTML for analysis (keep only menu section)."""
    if not html_content:
        return ""

    original_size = len(html_content)
    soup = BeautifulSoup(html_content, "html.parser")

    menu_container = soup.find("div", {"data-nclicks-area-code": "bmv"})
    if menu_container:
        cleaned_html = str(menu_container)
    else:
        for element in soup(["script", "style", "svg"]):
            element.decompose()
        cleaned_html = str(soup.body) if soup.body else str(soup)

    reduced = 100 - (len(cleaned_html) / original_size * 100)
    # print(f"   - Cleaned HTML reduced by {reduced:.1f}%")
    # this is specific to 1 type of naver menu structure. Could use a moe generic cleaning function without
    # using data-nclicks-area-code
    return cleaned_html


async def fetch_iframe_html(url: str):
    """Fetch iframe HTML from Naver Maps."""
    browser = None
    try:
        # print("Launching browser...")
        browser = await launch(
            {"headless": True, "args": ["--no-sandbox", "--disable-setuid-sandbox"]}
        )
        page = await browser.newPage()
        await page.setUserAgent(
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
            "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
        )

        await page.goto(url, {"waitUntil": "networkidle0", "timeout": 60000})
        # print("Page loaded, locating iframe...")

        iframe_selector = "iframe#entryIframe"
        await page.waitForSelector(iframe_selector, {"timeout": 30000})
        iframe_element = await page.querySelector(iframe_selector)
        frame = await iframe_element.contentFrame()

        if not frame:
            # print("No iframe content found")
            return None

        await frame.waitForSelector("body", {"timeout": 10000})
        html = await frame.content()
        # print("Extracted iframe HTML")
        return html
    except Exception as e:
        # print(f"Error: {e}")
        return None
    finally:
        if browser:
            await browser.close()
            # print("Browser closed.")


# Need more robust extraction for custom cases
def extract_menu_from_html(html_content: str):
    """Extract menu items directly from HTML using CSS selectors."""
    soup = BeautifulSoup(html_content, "html.parser")
    menu_items = []
    for item in soup.select("li.E2jtL"):
        name = item.select_one("span.lPzHi")
        price = item.select_one("div.GXS1X em")

        if name:
            menu_items.append({
                "name": name.get_text(strip=True),
                "price": price.get_text(strip=True) if price else None
            })
    return menu_items


async def main():
    print("Cafe Menu Scraper (City-based Scraping with Slicing)")
    print("=" * 60)

    # Cities with slice notation: 'city_name' or 'city_name[start:end]'
    cities = [
        # '강릉',
        # '광주',
        # '대구',
        # '대전',
        # '부산',
        # '서울[:1000]',
        # '서울[1000:2000]',
        # '서울[2000:3000]',
        # '서울[3000:4000]',
        # '서울[4000:5000]',
        # '세종',
        # '울산',
        # '인천',
        # '전주',
        # '제주',
        # '창원',
        # '천안',
        # '청주',
        # '춘천',
        # '포항',

        # cities that I forgot
        # "강원",  #      1
        # "경북",  #    761
        # "전남",  #    158
        # "전북",  #    823
        # "충남",  #    948
        # "충북",  #    802

        # The below 2 are somehow not present
        # Update: the 2 missing province gyeonggi and gyeong nam have now been update with cities within it.
        # "경기",
        # "경남",

        "거제",
        "고양",
        "김해",
        "성남",
        "수원",
        "용인",
        "진주",
    ]
    
    try:
        df = pd.read_csv("../data/remaining_naver_cafes_api_scraped2.csv", encoding='utf-8-sig')
        # df = pd.read_csv("../data/naver_cafes_api_scraped_v2.csv", encoding='utf-8-sig')
        # df = pd.read_csv("../data/scraped_output4.csv", encoding='utf-8-sig')
        
        # Initialize columns if they don't exist
        if 'success' not in df.columns:
            df['success'] = ''
        if 'americano' not in df.columns:
            df['americano'] = ''
        if 'menu_items' not in df.columns:
            df['menu_items'] = ''

        print(f"Loaded {len(df)} cafes from file.")
    except Exception as e:
        print(f"Error loading file: {e}")
        return

    total_processed = 0
    total_success = 0
    total_failed = 0
    total_skipped = 0

    for city_spec in cities:
        # Parse city and slice
        if '[' in city_spec:
            city_name = city_spec.split('[')[0]
            slice_str = city_spec.split('[')[1].rstrip(']')
            
            # Parse slice notation
            parts = slice_str.split(':')
            start = int(parts[0]) if parts[0] else None
            end = int(parts[1]) if len(parts) > 1 and parts[1] else None
        else:
            city_name = city_spec
            start = None
            end = None

        print(f"\nProcessing: {city_name}" + (f" [{start}:{end}]" if start or end else ""))
        print("-" * 60)

        # Filter cafes by city
        city_df = df[df['city'] == city_name].copy()
        
        # Apply slicing
        if start is not None or end is not None:
            city_df = city_df.iloc[start:end]
        
        city_skipped = 0
        city_success = 0
        city_failed = 0

        # Progress bar
        pbar = tqdm(city_df.iterrows(), total=len(city_df), desc=f"{city_name}")
        
        for idx, row in pbar:
            cafe_name = row["name"]
            base_url = row["detail_url"]
            place_id = row["place_id"]
            success_status = row["success"]

            # Skip cafes already marked as 'true'
            if success_status == "true":
                city_skipped += 1
                total_skipped += 1
                continue

            menu_url = base_url + "?c=15.00,0,0,0,dh&placePath=/menu?"
            html_content = await fetch_iframe_html(menu_url)

            americano_flag = False
            menu_items = []

            if html_content:
                cleaned_html = clean_html_for_gpt(html_content)

                # Check for 'americano'
                if "아메리카노" in cleaned_html:
                    americano_flag = True
                
                menu_items = extract_menu_from_html(cleaned_html)

                if menu_items:
                    df.loc[idx, "success"] = "true"
                    df.loc[idx, "americano"] = str(americano_flag)
                    df.loc[idx, "menu_items"] = json.dumps(menu_items, ensure_ascii=False)
                    city_success += 1
                    total_success += 1
                else:
                    df.loc[idx, "success"] = "later"
                    city_failed += 1
                    total_failed += 1
            else:
                df.loc[idx, "success"] = "later"
                city_failed += 1
                total_failed += 1

            total_processed += 1
            
            # Update progress bar
            pbar.set_postfix({
                'success': city_success,
                'failed': city_failed,
                'skipped': city_skipped
            })

            delay = random.uniform(1.5, 3.5)
            await asyncio.sleep(delay)

        print(f"\n{city_name} Summary: Success={city_success}, Failed={city_failed}, Skipped={city_skipped}")

    # Save the final results
    # print(df)
    # i added a drop step so that it is easier to merge, MAy need to add a drop step 
    # before to conserve memory or use checkpoint files "scraped_outputN.csv"
    df = df[df['success'].isin(['true', 'later'])] 

    df.to_csv("../data/scraped_output15.csv", index=False, encoding='utf-8-sig')
    
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(f"Total Processed: {total_processed}")
    print(f"Total Success:   {total_success}")
    print(f"Total Failed:    {total_failed}")
    print(f"Total Skipped:   {total_skipped}")
    print(f"\nResults saved to: scraped_output15.csv")


if __name__ == "__main__":
    nest_asyncio.apply()
    asyncio.run(main())


# GPT as a backup
def extract_menu_with_gpt(html_content: str):
    """
    Extract menu items using GPT with your specified API style.
    """
    if not html_content:
        return []
        
    try:
        # print("Sending cleaned HTML to GPT for menu extraction...")

        prompt = f"""
                    You are an expert data extraction AI. From the HTML below, extract menu items and their prices.

                    RULES:
                    1. Output MUST be a JSON object with one key: "menu_items".
                    2. "menu_items" must be a list of objects: {{"name": <string>, "price": <string>}}.
                    3. Keep price ranges as-is (e.g. "5,000~5,500원").
                    4. If no menu items found, return: {{"menu_items": []}}.
                    5. Output ONLY the JSON. Nothing else.

                    HTML:
                    {html_content}
                    """

        response = client.responses.create(
            model="gpt-4o-mini",
            input=prompt,
            max_output_tokens=2000,
            temperature=0.0
        )

        response_text = response.output_text.strip()
        # print(f"Received {len(response_text)} characters from GPT")

        try:
            if response_text.startswith("```json"):
                response_text = response_text[7:-3].strip()
            
            parsed = json.loads(response_text)
            menu_items = parsed.get("menu_items", [])
            # print(f"Extracted {len(menu_items)} menu items")
            return menu_items
        except json.JSONDecodeError as e:
            # print(f"JSON parse error: {e}")
            # print(f"Raw GPT output (first 200 chars): {response_text[:200]}...")
            return []

    except Exception as e:
        # print(f"GPT error: {e}")
        return []