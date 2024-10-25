# Written by Delwen L. Franzen

import pandas as pd
import requests
import requests_cache
import time
import json

from ratelimit import limits, sleep_and_retry


@sleep_and_retry
@limits(calls=1, period=0.5)
def call_api_server(url):
    now = time.ctime(int(time.time()))
    response = requests.get(url)

    if response.status_code != 200:
        raise Exception('API response: {}'.format(response.status_code))
    return response.json()


def call_api(url):
    # req = requests.Request('GET', url)
    #
    # cache = requests_cache.get_cache()
    #
    # prepped = requests.Session().prepare_request(req)
    # cache_key = cache.create_key(prepped)
    #
    # try:
    #     response = cache.get_response(cache_key)
    # except (ImportError, TypeError):
    #     response = None
    #
    # if response:
    #     return response.json()

    return call_api_server(url)


# https://github.com/ecrin-github/rms-portal-new/blob/504c09340f6af24f42c6d9996da9cbf8a458beeb/src/app/pages/common/study/upsert/upsert-study/upsert-study.component.ts#L428
# DRKS 100124
# ctgov 100120
# euctr 100123
# pubmed 100135


def resolve_ecrin_studyid(trn: str, registry: str):
    # map registry to number
    registry_map = {'ClinicalTrials.gov': 100120, 'DRKS': 100124, 'EUCTR': 100123}

    registry_id = registry_map[registry]
    url = f"https://crmdr.ecrin.org/api/Study/MDRId/{registry_id}/{trn}"
    try:
        output = call_api(url)
    except Exception as e:
        print("Error", e)
        return None
    return output


def get_study_info(study_id):

    url = f"https://crmdr.ecrin.org/api/Study/{study_id}"
    try:
        output = call_api(url)
    except Exception as e:
        print("Error", e)
        return None
    return output


def parse_identifiers(api_response):
    if not api_response:
        return None
    if len(api_response) > 1:
        print("API response has more than one entry!")
    result = []
    # to handle cases where more than one entry in API response although no idea why that should happen
    for index in range(0, len(api_response)):
        study_summary = json.loads(api_response[index])
        study_identifiers = study_summary['study_identifiers']
        for entry in study_identifiers:
            id = entry['identifier_value']
            id_type = entry['identifier_type']['name']
            id_type_id = entry['identifier_type']['id']
            source_name = entry['source']['name']
            result.append((id, id_type, id_type_id, source_name))
    return result


def main():
    data = pd.read_csv("data/iv_trials_dashboard.csv")
    intovalue_ids = data.drop_duplicates(subset=['id'])
    intovalue_ids = intovalue_ids[['id', 'registry']]

    print(len(intovalue_ids))
    print(pd.unique(intovalue_ids['registry']))

    # Get first n elements for testing
    # intovalue_ids = intovalue_ids[0:500]

    table = []
    # keep track of errors thrown
    errors = []
    for index, row in intovalue_ids.iterrows():
        trn = row['id']
        registry = row['registry']

        study_id = resolve_ecrin_studyid(trn, registry)
        if study_id is None:
            print(f"For {trn} Error getting MDR study id!")
            continue
        print(f"{trn} [{registry}] -> {study_id}")

        study_info = get_study_info(study_id)
        # added handling of specific study info
        if study_info is None or study_info == ['']:
            print(f"For {study_id} Error getting study information!")
            errors.append((trn, registry, study_id))
            continue

        identifiers = parse_identifiers(study_info)
        if identifiers is None:
            print(f"For {study_id} Error getting identifiers!")
            continue
        for entry in identifiers:
            # skip self-references
            if entry[0] == trn:
                continue
            identifier = entry[0]
            identifier_type = entry[1]
            identifier_type_id = entry[2]
            identifier_source = entry[3]
            table.append((trn, registry, study_id, identifier, identifier_type, identifier_type_id, identifier_source))

    # Create a dataframe to store the results
    df = pd.DataFrame(table, columns=[
        'iv_id', 'iv_registry', 'mdr_study_id',
        'identifier', 'identifier_type', 'identifier_type_id', 'identifier_source'])
    print(df)
    df.to_csv("data/mdr_identifiers.csv", index=False)

    df_errors = pd.DataFrame(errors, columns=[
        'iv_id', 'iv_registry', 'mdr_study_id'])
    df_errors.to_csv("data/mdr_identifiers_errors.csv", index=False)


if __name__ == '__main__':
    main()
