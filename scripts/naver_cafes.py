import requests
import time
import json
import random
import os
from datetime import datetime
from typing import List, Dict, Tuple
import pandas as pd
from math import ceil
import math


# Configuration
CHECKPOINT_FILE = "../data/api_scraper_checkpoint.json"
OUTPUT_CSV = "../data/seoul_naver_cafes_api_scraped.csv"
RAW_RESPONSES_DIR = "../data/raw_api_responses"


# API Configuration
BASE_URL = "https://map.naver.com/p/api/smart-around/places"
HEADERS = {
    "User-Agent": ("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
                   "(KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36"),
    "Accept": "application/json, text/plain, */*",
    "Accept-Language": "ko-KR,ko;q=0.8,en-US;q=0.6,en;q=0.4",
    "Referer": "https://map.naver.com/p/",
    "Cache-Control": "no-cache",
    "Pragma": "no-cache"
}

# City center coordinates
CITY_CENTERS = {
    # "울산": {"lat": 35.5384, "lng": 129.3114},
    # "부산": {"lat": 35.1796, "lng": 129.0756},
    # "서울": {"lat": 37.5665, "lng": 126.9780},
    # "대구": {"lat": 35.8714, "lng": 128.6014},
    # "인천": {"lat": 37.4563, "lng": 126.7052},
    # "광주": {"lat": 35.1595, "lng": 126.8526},
    # "대전": {"lat": 36.3504, "lng": 127.3845},
    # "세종": {"lat": 36.4800, "lng": 127.2890},
    # "춘천": {"lat": 37.8813, "lng": 127.7298},
    # "강릉": {"lat": 37.7519, "lng": 128.8761},
    # "천안": {"lat": 36.8151, "lng": 127.1139},
    # "청주": {"lat": 36.6424, "lng": 127.4890},
    # "전주": {"lat": 35.8242, "lng": 127.1479},
    # "창원": {"lat": 35.2272, "lng": 128.6811},
    # "포항": {"lat": 36.0190, "lng": 129.3435},
    # "제주": {"lat": 33.4996, "lng": 126.5312},




    # We have grdp data but we forgot
    "충남": {"lat": 36.8145, "lng": 127.1469},
    "전남": {"lat": 34.9917, "lng": 126.7157},
    "충북": {"lat": 36.6424, "lng": 127.4890},
    "경북": {"lat": 36.5656, "lng": 128.7260},
    "경기": {"lat": 37.4138, "lng": 127.5183},
    "경남": {"lat": 35.4606, "lng": 128.2132},
    "강원": {"lat": 37.8228, "lng": 128.1555},
    "전북": {"lat": 35.8251, "lng": 127.1500}
}


def load_checkpoint():
    """Load progress from checkpoint file."""
    if os.path.exists(CHECKPOINT_FILE):
        with open(CHECKPOINT_FILE, 'r', encoding='utf-8') as f:
            return json.load(f)
    return {"completed_boxes": [], "cafes": {}}


def save_checkpoint(data):
    """Save progress to checkpoint file."""
    with open(CHECKPOINT_FILE, 'w', encoding='utf-8') as f:
        json.dump(data, f, ensure_ascii=False, indent=2)
    print(f" Checkpoint: {len(data['cafes'])} unique cafes")


def generate_grid_boxes(center_lat, center_lng, radius_km, box_size_km):


    lat_deg_per_km = 1 / 111.0
    lng_deg_per_km = 1 / (111.0 * math.cos(math.radians(center_lat)))
    
    num_boxes_per_side = ceil((2 * radius_km) / box_size_km)
    start_lat = center_lat - (radius_km * lat_deg_per_km)
    start_lng = center_lng - (radius_km * lng_deg_per_km)
    
    boxes = []
    box_id = 0
    
    for i in range(num_boxes_per_side):
        for j in range(num_boxes_per_side):

            sw_lat = start_lat + (i * box_size_km * lat_deg_per_km)
            sw_lng = start_lng + (j * box_size_km * lng_deg_per_km)
            ne_lat = sw_lat + (box_size_km * lat_deg_per_km)
            ne_lng = sw_lng + (box_size_km * lng_deg_per_km)
            
            # Box center (for searchCoord parameter)
            box_center_lat = (sw_lat + ne_lat) / 2
            box_center_lng = (sw_lng + ne_lng) / 2
            
            boxes.append({
                'id': box_id,
                'center_lat': box_center_lat,
                'center_lng': box_center_lng,
                'sw_lat': sw_lat,
                'sw_lng': sw_lng,
                'ne_lat': ne_lat,
                'ne_lng': ne_lng
            })
            box_id += 1
    
    return boxes


def call_smart_around_api(box, limit=50, code="01", sort_type="RECOMMEND"):

    """
    Call the smart-around/places API for a bounding box.
    
    Args:
        box: Bounding box dict with coordinates
        limit: Max results per call (50-100 recommended)
        code: Category code (01 = cafes)
        sort_type: RECOMMEND, DISTANCE, etc.
    
    Returns:
        API response dict



    Working GET request example below:
      {
    "url": "https://map.naver.com/p/api/smart-around/places?searchCoord=129.080971%3B35.227764&boundary=129.06619733880325%3B35.22020030186367%3B129.09595913003272%3B35.235151728953625&code=01&limit=20&sortType=RECOMMEND&timeCode=AFTERNOON",
    "method": "GET",
    "headers": {
      "Accept": "application/json, text/plain, */*",
      "Accept-Language": "ko-KR,ko;q=0.8,en-US;q=0.6,en;q=0.4",
      "Cache-Control": "no-cache",
      "Expires": "Sat, 01 Jan 2000 00:00:00 GMT",
      "Pragma": "no-cache",
      "Referer": "https://map.naver.com/p?c=15.00,0,0,0,dh",
      "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36",
      "sec-ch-ua": "\"Chromium\";v=\"140\", \"Not=A?Brand\";v=\"24\", \"Google Chrome\";v=\"140\"",
      "sec-ch-ua-mobile": "?0",
      "sec-ch-ua-platform": "\"Linux\""
    },
    "post_data": ""
  },

    Note: Can't test on POSTMAN due to response size. 
    """
    # Format coordinates for API
    search_coord = f"{box['center_lng']};{box['center_lat']}"
    boundary = f"{box['sw_lng']};{box['sw_lat']};{box['ne_lng']};{box['ne_lat']}"
    
    params = {
        'searchCoord': search_coord,
        'boundary': boundary,
        'code': code,
        'limit': str(limit),
        'sortType': sort_type,
        'timeCode': 'AFTERNOON' # cafes open in the afternoon
    }
    
    try:
        response = requests.get(BASE_URL, params=params, headers=HEADERS, timeout=30)
        
        if response.status_code == 200:
            return response.json()
        else:
            print(f" API returned status {response.status_code}")
            return None
            
    except Exception as e:
        print(f" error: {e}")
        return None


def parse_cafe_data(api_response):
    cafes = []
    
    if not api_response:
        return cafes
    
    # The data is usually in a 'places' or 'items' key
    items = api_response["result"]["list"]

    # grab the totalCount as well per call
    # totalCount += api_response["result"]["meta"]["totalCount"]

    for item in items:
        cafe = {
            'place_id': item.get('id', ''),
            'rank': item.get('rank', ''),
            'name': item.get('name', ''),
            'x': item.get('x', ''),
            'y': item.get('y', ''),
            'distance': item.get('distance', ''),
            'categoryName': item.get('categoryName', ''),
            'categoryPath': json.dumps(item.get('categoryPath', []), ensure_ascii=False),
            'category': json.dumps(item.get('category', []), ensure_ascii=False),
            'reviewCount': item.get('reviewCount', 0),
            'averagePrice': item.get('averagePrice', ''),
            'address': item.get('address', ''),
            'roadAddress': item.get('roadAddress', ''),
            'abbrAddress': item.get('abbrAddress', ''),
            'shortAddress': json.dumps(item.get('shortAddress', []), ensure_ascii=False),
            'displayContext': json.dumps(item.get('displayContext', []), ensure_ascii=False),
            'labelTheme': item.get('labelTheme', ''),
            'microReviews': json.dumps(item.get('microReviews', []), ensure_ascii=False),
            'cardDescription': item.get('cardDescription', ''),
            'description': item.get('description', ''),
            'images': json.dumps(item.get('images', []), ensure_ascii=False),
            'marker': item.get('marker', ''),
            'markerSelected': item.get('markerSelected', ''),
            'markerId': item.get('markerId', ''),
            'bizhourInfo': item.get('bizhourInfo') or '',
            'hasBroadcastInfo': item.get('hasBroadcastInfo', False),
            'broadcastInfo': item.get('broadcastInfo') or '',
            'michelinGuide': item.get('michelinGuide') or '',
            'reservation': item.get('reservation') or '',
            'hasSmartOrder': item.get('hasSmartOrder', False),
            'hasNPay': item.get('hasNPay', False),
            'reservationLabel': json.dumps(item.get('reservationLabel', {}), ensure_ascii=False),
            'contentsInfo': item.get('contentsInfo') or '',
            'ratingInfo': json.dumps(item.get('ratingInfo') or {}, ensure_ascii=False),
            'reviewDisplay': json.dumps(item.get('reviewDisplay', []), ensure_ascii=False),
            'detail_url': f"https://map.naver.com/p/entry/place/{item.get('id', '')}" if item.get('id') else ''
        }

        if cafe['place_id']:
            cafes.append(cafe)

    return cafes




def scrape_city(city_name,center_coords, radius_km, box_size_km ):
    """
    Scrape all cafes in a city using grid approach.
    
    Args:
        city_name: Name of the city
        center_coords: Dict with 'lat' and 'lng' keys
        radius_km: Radius to cover around center
        box_size_km: Size of each grid box
    
    Returns:
        List of all cafes found
    """
    print("\t\t")
    print(f"City: {city_name}")
    # print(f"Center: {center_coords['lat']:.4f}, {center_coords['lng']:.4f}")
    print(f"Coverage: {radius_km} km radius")
    print("\t\t")
    total_counts = {}
    
    # Generate grid
    boxes = generate_grid_boxes(
        center_coords['lat'], 
        center_coords['lng'],
        radius_km=radius_km,
        box_size_km=box_size_km
    )
    
    print(f"{len(boxes)} bounding boxes")
    
    all_cafes = []
    
    for i, box in enumerate(boxes, 1):
        print(f"\n[{i}/{len(boxes)}] Box #{box['id']}")
        # print(f"Center: {box['center_lat']:.4f}, {box['center_lng']:.4f}")
        
        # Limit set to 50 per box, but expanding the bounding box suggest that the call can be scale up to 1700
        # considering to small test a limit of say 10 and run for all cities to get the totalCount per bounding box,
        # then perhaps can apply a matrix/dictionary to capture say 80% of all cafes, otherwise high dense areas we 
        # are only capturing. Alternatively, doing 400 instead of 16 bounding boxes, longer run time but granular results
        # NVM, naver has a soft limit of 100
        response = call_smart_around_api(box, limit=100)

        
        if response:
            # Parse cafes
            cafes = parse_cafe_data(response)
            total_counts[box['id']] = response["result"]["meta"]["totalCount"]
            # Add metadata
            for cafe in cafes:
                cafe['city'] = city_name
                cafe['box_id'] = box['id']
                cafe['scraped_at'] = datetime.now().isoformat()
            
            all_cafes.extend(cafes)
            print(f"{len(cafes)} found ")
            
            # Save raw response for debugging
            if not os.path.exists(RAW_RESPONSES_DIR):
                os.makedirs(RAW_RESPONSES_DIR)
            
            raw_file = f"{RAW_RESPONSES_DIR}/{city_name}_box{box['id']}.json"
            with open(raw_file, 'w', encoding='utf-8') as f:
                json.dump(response, f, ensure_ascii=False, indent=2)
        else:
            print(f"No data returned")
        
        # Rate limiting 
        if i < len(boxes):
            delay = random.uniform(0.5, 1.5)
            time.sleep(delay)
    
    print(f"\n City name: num of cafes {city_name}: {len(all_cafes)}")
    return all_cafes, total_counts


def deduplicate_cafes(cafes: List[Dict]) -> pd.DataFrame:
    """
    Convert to DataFrame and remove duplicates.
    
    Args:
        cafes: List of cafe dicts
    
    Returns:
        Deduplicated DataFrame
    """
    df = pd.DataFrame(cafes)
    
    if df.empty:
        return df
    
    original_count = len(df)
    
    # Remove duplicates based on place_id
    df = df.drop_duplicates(subset=['place_id'], keep='first')
    
    duplicates_removed = original_count - len(df)
    if duplicates_removed > 0:
        print(f"🔄 Removed {duplicates_removed} duplicate cafes")
    
    return df


def main():
    CITIES_TO_SCRAPE = {
        # "울산": {"radius_km": 1.0, "box_size_km": 0.5},
        # "부산": {"radius_km": 1.0, "box_size_km": 0.5},
        # "서울": {"radius_km": 1.0, "box_size_km": 0.1},
        # "대구": {"radius_km": 1.0, "box_size_km": 0.5},
        # "인천": {"radius_km": 1.0, "box_size_km": 0.5},
        # "광주": {"radius_km": 1.0, "box_size_km": 0.5},
        # "대전": {"radius_km": 1.0, "box_size_km": 0.5},
        # "세종": {"radius_km": 1.0, "box_size_km": 0.5},
        # "춘천": {"radius_km": 1.0, "box_size_km": 0.5},
        # "강릉": {"radius_km": 1.0, "box_size_km": 0.5},
        # "천안": {"radius_km": 1.0, "box_size_km": 0.5},
        # "청주": {"radius_km": 1.0, "box_size_km": 0.5},
        # "전주": {"radius_km": 1.0, "box_size_km": 0.5},
        # "창원": {"radius_km": 1.0, "box_size_km": 0.5},
        # "포항": {"radius_km": 1.0, "box_size_km": 0.5},
        # "제주": {"radius_km": 1.0, "box_size_km": 0.5},


        # Have GRDP Data but forgot

        "충남": {"radius_km": 1.0, "box_size_km": 0.5},
        "전남": {"radius_km": 1.0, "box_size_km": 0.5},
        "충북": {"radius_km": 1.0, "box_size_km": 0.5},
        "경북": {"radius_km": 1.0, "box_size_km": 0.5},
        "경기": {"radius_km": 1.0, "box_size_km": 0.5},
        "경남": {"radius_km": 1.0, "box_size_km": 0.5},
        "강원": {"radius_km": 1.0, "box_size_km": 0.5},
        "전북": {"radius_km": 1.0, "box_size_km": 0.5}

    }

    
    # Load checkpoint
    checkpoint_data = load_checkpoint()
    all_cafes_dict = checkpoint_data.get('cafes', {})
    
    print(f"Checkpoint: {len(all_cafes_dict)} unique cafes loaded")
    
    # Scrape each city
    for city_name, config in CITIES_TO_SCRAPE.items():
        
        # Scrape city
        cafes, total_counts = scrape_city(
            city_name,
            CITY_CENTERS[city_name],
            radius_km=config['radius_km'],
            box_size_km=config['box_size_km']
        )

        # Save total_counts for this city to a JSON file
        total_counts_file = f"{RAW_RESPONSES_DIR}/{city_name}_total_counts.json"
        os.makedirs(RAW_RESPONSES_DIR, exist_ok=True)
        with open(total_counts_file, 'w', encoding='utf-8') as f:
            json.dump(total_counts, f, ensure_ascii=False, indent=2)


        # Add to checkpoint (using place_id as key for deduplication)
        for cafe in cafes:
            place_id = cafe['place_id']
            if place_id not in all_cafes_dict:
                all_cafes_dict[place_id] = cafe
        
        # Save checkpoint after each city
        checkpoint_data['cafes'] = all_cafes_dict
        save_checkpoint(checkpoint_data)
        
        # Rate limit between cities
        if len(CITIES_TO_SCRAPE) > 1:
            print(f"\n Sleeping before next city...")
            time.sleep(random.uniform(3, 5))
    

    all_cafes_list = list(all_cafes_dict.values())
    
    if all_cafes_list:
        df = pd.DataFrame(all_cafes_list)
        

        df = df.sort_values(['city', 'name'])
        

        df.to_csv(OUTPUT_CSV, index=False, encoding='utf-8-sig')
        print(f"\nSaved {len(df)} unique cafes → {OUTPUT_CSV}")
        

        print(f"\n Summary by City:")
        print(df.groupby('city')['place_id'].count())
        
    else:
        print("\n  No cafes collected")
    
    # Clean up checkpoint
    if os.path.exists(CHECKPOINT_FILE):
        os.remove(CHECKPOINT_FILE)

    


    print(f"Total unique cafes: {len(all_cafes_list)}")
    print(f"Cities covered: {len(CITIES_TO_SCRAPE)}")


if __name__ == "__main__":
    main()