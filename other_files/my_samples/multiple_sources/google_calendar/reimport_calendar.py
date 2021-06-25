#!/usr/bin/python3

from apiclient.discovery import build
from apiclient.http import BatchHttpRequest
from googleapiclient.errors import HttpError
from httplib2 import Http
from oauth2client import file, client, tools
from time import sleep
from random import randrange
from os.path import isfile
import json, sys, logging, traceback, datetime, os, pathlib, logging

def init_run_folder():
    os.chdir(pathlib.Path(__file__).parent.absolute())
    folder = datetime.datetime.now().strftime('%Y-%m-%d__%H:%M:%S.%f')
    os.mkdir(folder)
    return folder

def init_logging(log_file):
    class StdoutFilter(logging.Filter):
        def filter(self, rec):
            # return rec.levelno in (logging.INFO, logging.DEBUG)
            return rec.levelno == logging.INFO
    file_handler = logging.FileHandler(log_file)
    file_handler.setLevel(logging.DEBUG)
    stdout_handler = logging.StreamHandler(sys.stdout)
    stdout_handler.setLevel(logging.DEBUG)
    stdout_handler.addFilter(StdoutFilter())
    stderr_handler = logging.StreamHandler()
    stderr_handler.setLevel(logging.WARNING)
    logging.basicConfig(level = logging.DEBUG, datefmt = '%Y-%m-%d %H:%M:%S',
        format = '%(asctime)s.%(msecs)03d %(levelname)s: %(message)s',
        handlers = [
            file_handler,
            stdout_handler,
            stderr_handler,
        ],
    )

def start(activity):
    logging.info(f'{activity}...')
    return activity

def end(activity):
    logging.info(f'{activity}: done.')

def get_calendar_service():
    activity = start('Getting google calendar service')

    # storage.json will be created after successfull login in google account (in
    # popped-up web browser window) after executing tools.run_flow() (see
    # below).
    storage = 'storage.json'
    store = file.Storage(storage)
    creds = store.get() if isfile(storage) else None

    if not creds or creds.invalid:

        # See scopes description here:
        # https://developers.google.com/calendar/auth#OAuth2Authorizing
        # This scope allows:
        # See, edit, share, and permanently delete all the calendars you can
        # access using Google Calendar.
        scope = 'https://www.googleapis.com/auth/calendar'

        # To obtain client_secret.json you need to do the following:
        # 1) Visit https://console.developers.google.com and create new project
        #    (external user type).
        # 2) Enable Google Calendar API for this newly created project.
        # 3) Partially configure OAuth consent screen:
        #    3.1) fill Application name
        #    3.2) add scope: ../auth/calendar.events
        # 4) Create credentials (OAuth client ID).
        # 5) Download client_secret.json.
        # flow = client.flow_from_clientsecrets('client_secret.json', scope)
        #
        # Embed data from client_secret.json in this script instead:
        flow = client.OAuth2WebServerFlow(
                client_id = '237566182630-l1cn7tvuk7ajir2rh7l6um87ftnvjv56.apps.googleusercontent.com',
                client_secret = '3Z9A6wepP6AV-nKzCBWo5ePl',
                scope = scope,
                auth_uri = 'https://accounts.google.com/o/oauth2/auth',
                token_uri = 'https://oauth2.googleapis.com/token'
                )

        # The following will create storage.json:
        creds = tools.run_flow(flow, store)

    # "cache_discovery = False" avoids WARNING:
    # file_cache is unavailable when using oauth2client >= 4.0.0 or google-auth
    # Another solution to silence the warning:
    # logging.getLogger('googleapiclient.discovery').setLevel(logging.CRITICAL)
    ret = build('calendar', 'v3', http = creds.authorize(Http()),
            cache_discovery = False)
    end(activity)
    return ret

def execute(func):
    # Sometimes google API throw an exception "Rate Limit Exceeded".
    # See exception description:
    # https://developers.google.com/analytics/devguides/reporting/core/v3/errors
    # Workaround: implement exponential backoff:
    # https://developers.google.com/analytics/devguides/reporting/core/v3/errors#backoff
    # Wait  1s + random_number_milliseconds seconds, retry
    # Wait  2s + random_number_milliseconds seconds, retry
    # Wait  4s + random_number_milliseconds seconds, retry
    # Wait  8s + random_number_milliseconds seconds, retry
    # Wait 16s + random_number_milliseconds seconds, retry
    # If you still get an error, stop and log the error.
    sec = 1
    while sec <= 1024: # 16
        try:
            ret = func()
            return ret # func() succeeded
        except HttpError as e:
            if e.resp.status != 403:
                raise # got unexpected exception => rethrow it
            reason = e._get_reason()
            prefix = "Quota exceeded for quota group 'default' and limit \
'Queries per user per 100 seconds' of service 'calendar-json.googleapis.com' \
for consumer 'project_number:"
            if reason != 'Rate Limit Exceeded' and not prefix in reason:
                raise # got unexpected exception => rethrow it
            ms = randrange(1000)
            activity = start(f'Queries rate limit exceeded => waiting for \
{sec} seconds')
            sleep(sec + ms / 1000)
            end(activity)
            sec = sec * 2 # exponential backoff
    raise Exception('Retry attempts exceeded (exponential backoff)')

def get_all_events(service):
    activity = start('Reading all events from google calendar')
    page_token = None
    all_events = []
    total = 0
    iteration = 0
    while True:
        def _get_all_events():
            return service.events().list(calendarId = 'primary',
                    # showDeleted = True,
                    # singleEvents = True,
                    pageToken = page_token, maxResults = 2500).execute()
        events = execute(_get_all_events)
        events_i = events['items']
        if events_i:
            # for event in events_i:
            #     print(event['summary'])
            #     if event.get('colorId'):
            #         print(event)
            #         print(event['colorId'])
            num = len(events_i)
            e_from = total + 1
            total = total + num
            logging.info(f'Events [{e_from}; {total}] have been read...')
            all_events += events_i
        else:
            iteration = iteration + 1
            logging.info(f'Reading events from google calendar is in progress \
(iteration #{iteration})...')
        # See: https://developers.google.com/calendar/v3/pagination
        page_token = events.get('nextPageToken')
        if not page_token:
            break
    end(activity)
    return all_events

def update_events(service, events):
    main_activity = start('Updating events')
    num = 0
    total = 0 # len(events)
    for event in events:
        if event['status'] != 'cancelled':
            total = total + 1
    num_in_batch = 0
    num_from = 0
    max_requests_in_batch = 1000
    batch = None
    activity = None
    def update_event(request_id, response, exception):
        if exception:
            raise exception
    def next_batch():
        nonlocal activity, batch, num_from
        activity = start('Creating batch request for updating events')
        batch = service.new_batch_http_request(callback = update_event)
        num_from = num
    def batch_execute():
        def _update_events():
            batch.execute()
        nonlocal activity
        end(activity)
        activity = start(f'Updating events [{num_from}; {num}] of {total}')
        execute(_update_events)
        end(activity)
    for event in events:
        if event['status'] == 'cancelled':
            continue
        num = num + 1
        num_in_batch = num % max_requests_in_batch
        if num_in_batch == 1:
            next_batch()
        # We don't change anything in events. By executing the following request
        # we hope to set event['updated'] to current datetime, which hopefully
        # will force old events to appear in the following android application:
        # https://play.google.com/store/apps/details?id=com.concentriclivers.calendar
        # Unfortunately it looks like if we change event['updated'] explicitely,
        # the change is ignored.
        batch.add(service.events().update(calendarId = 'primary',
            eventId = event['id'], body = event))
        if num_in_batch == 0:
            batch_execute()
    # Execute last batch:
    if num_in_batch != 0:
        batch_execute()
    end(main_activity)

def save_events(events, file_path):
    activity = start(f'Saving events to file {file_path}')
    with open(file_path, 'w', encoding = 'utf-8') as f:
        f.write(json.dumps(events, indent = 4, ensure_ascii = False))
    end(activity)

def delete_all_events(service):
    activity = start('Deleting all events from google calendar')
    def _delete_all_events():
        iteration = 0
        while True:
            iteration = iteration + 1
            try:
                service.calendars().clear(calendarId = 'primary').execute()
                break
            except HttpError as e:
                if e.resp.status != 503:
                    raise # got unexpected exception => rethrow it
                reason = e._get_reason()
                if reason != 'The service is currently unavailable.':
                    raise
                logging.info(f'Got error 503: "{reason}". Retrying (iteration \
#{iteration})...')
    execute(_delete_all_events)
    end(activity)

def load_events(file_path):
    activity = start(f'Reading and parsing events from {file_path}')
    with open(file_path, 'r', encoding = 'utf-8') as f:
        events = json.loads(f.read())
    end(activity)
    return events

def import_events(service, events):
    main_activity = start('Importing events')
    num = 0
    total = 0 # len(events)
    for event in events:
        if event['status'] != 'cancelled':
            total = total + 1
    num_in_batch = 0
    num_from = 0
    max_requests_in_batch = 1000
    batch = None
    activity = None
    def insert_event(request_id, response, exception):
        if exception:
            raise exception
    def next_batch():
        nonlocal activity, batch, num_from
        activity = start('Creating batch request for importing events to \
google calendar')
        batch = service.new_batch_http_request(callback = insert_event)
        num_from = num
    def batch_execute():
        def _import_events():
            batch.execute()
        nonlocal activity
        end(activity)
        activity = start(f'Importing events to google calendar \
[{num_from}; {num}] of {total}')
        execute(_import_events)
        end(activity)
    for event in events:
        if event['status'] == 'cancelled':
            continue
        num = num + 1

        # We should delete some IDs from event. Otherwise we will get a response
        # with HTTP error code 409 ("The requested identifier already exists.").
        del(event['id'])
        del(event['iCalUID'])

        # googleapiclient.errors.HttpError: <HttpError 400 when requesting
        # https://www.googleapis.com/calendar/v3/calendars/primary/events?alt=json
        # returned "Invalid sequence value. The specified sequence number is
        # below the current sequence number of the resource. Re-fetch the
        # resource and use its sequence number on the following request."
        # TODO can we hide new event modification with old one here due to
        # loosing sequence number?
        del(event['sequence'])

        # Per-event implementation:
#         activity = start(f'Importing event to google calendar {num} out of \
# {total}')
#         def _import_event():
#             service.events().insert(calendarId = 'primary',
#                     body = event).execute()
#         execute(_import_event)
#         end(activity)

        # Batch implementation:
        num_in_batch = num % max_requests_in_batch
        if num_in_batch == 1:
            next_batch()
        batch.add(service.events().insert(calendarId = 'primary', body = event))
        if num_in_batch == 0:
            batch_execute()
    # Execute last batch:
    if num_in_batch != 0:
        batch_execute()
    end(main_activity)

def main():
    try:
        ret = 0
        folder = init_run_folder()
        init_logging(os.path.join(folder, 'log.txt'))
        # events_file_name = 'all_events.json'
        # events_backup_file = os.path.join(folder, events_file_name)
        # events_import_file = os.path.join(os.getcwd(), events_file_name)
        service = get_calendar_service()
        all_events = get_all_events(service)
        update_events(service, all_events)
#         save_events(all_events, events_backup_file)
#         delete_all_events(service)
#         if isfile(events_import_file):
#             logging.info(f'Events will be reimported from file:\n\
# {events_import_file}')
#             all_events = load_events(events_import_file)
#         else:
#             logging.info('Events will be reimported from: google calendar')
#         import_events(service, all_events)
        logging.info('Success!')
    except Exception as e:
        ret = 1
        sys.stdout.flush()
        sys.stderr.flush()
        logging.error(traceback.format_exc())
        logging.error('Failure')
        sys.stderr.flush()
    finally:
        input('Press any key to exit...')
        sys.exit(ret)

if __name__ == '__main__':
    main()
