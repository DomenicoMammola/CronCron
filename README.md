# CronCron
This library implements a simple Cron-like system that can be embedded in any pascal application.
Somehow similar to (still developed?) Cromis library but without all its features but simple and it can handle a standard Cron schedule string in the format:
`
* * * * * *
| | | | | |
| | | | | +-- Year              (range: 1900-3000)
| | | | +---- Day of the Week   (range: 1-7, 1 standing for Monday)
| | | +------ Month of the Year (range: 1-12)
| | +-------- Day of the Month  (range: 1-31)
| +---------- Hour              (range: 0-23)
+------------ Minute            (range: 0-59)
`
