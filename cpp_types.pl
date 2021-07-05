% CPP types
%
%   Signal
%     id:
%     name:
%     cpp-type: { vehicle, building }
%     type: { numeric, enumeration, information, general-purpose }
%     sample-rate:
%     comment:
%
%     case type of
%       numeric:
%         format: (numeric formats)
%         min: (min signal value)
%         max: (max signal value)
%         resolution:  (according to format)
%         unit: (unit of the signal)
%       enumeration:
%         items: (string array of possible values)
%       information:
%         format: (representation format of values)
%       general-purpose:
%         *: (may be extended)
%
%   Measurement Channel
%     id:
%     name:
%     type: { time-series, histogram, geo-histogram,
%             basic-cpp-information, event-based, general-purpose }
%     comment:
%
%     case type of
%       time-series:
%         format: (format of the samples)
%         dimension: (opt int, dimension==1 assumed)
%         capture-interval: (interval between two samples in seconds)
%         on-change: (boolean, does MC only record changes)
%         sample-strategy: { min, max, average, last-known-value }
%         signal: array of Signal objects
%       histogram:
%         aggregation-strategy: { time, count, min, max }
%         capture-interval: (double, > 0, for one histogram)
%         dimensions: (int, dimensions of the Histogram)
%         bins: array of Bin-Configuration object (per dimension)
%       geo-histogram (only):
%         aggregation-strategy: { time, count, min, max }
%         capture-interval: (double, > 0, for one histogram)
%         dimensions: (int, dimensions of the Histogram)
%         bins: array of Bin-Configuration object (per dimension)
%         geo-resolution: (double, zoom level of the geo-histogram)
%       basic-cpp-information:
%         signal: (Signal object)
%       event-based:
%         format: (format of the samples)
%         event-sample-strategy:
%           { real-time-event, trigger-event, threshold-event }
%       general-purpose:
%          signal: (Signal object)
%
%   Data Package
%     data-package-id: (string UUID)
%     cvim-version: (e.g. 0.0.1 - 1.2.0, 1.2.1)
% *   type: { time-series, histogram, geo-histogram,
%             general-purpose, event-based, basic-cpp-information }
%     vault-id: (string UUID for cloud storage vault)
%     cpp-id: (string ID of the CPP)
%     cpp-type: { vehicle, building }
%     trip-id: (opt string from User)
%     room-id: (opt string room in a building)
% *   measurement-channel-id: (string id of the measurement channel)
%     mileage-start: (opt double)
%     mileage-stop: (opt double)
% *   geo-bounding-box: (Geo-Bounding box object)
%     location: (Location object)
%     oem-certification: (OEM-Certification object)
%     data-ownership-information: (Ownership-Information object)
%     expiration-date: (date-time)
%     data-masking-active: (boolean)
%
%     case type of
%       time-series:
% *       timestamp-start: (date-time)
% *       timestamp-stop: (date-time)
%         number-of-samples: (int)
%         statistic-properties: (opt statistic-properties object)
%         data: multi-dim array of time-series key-value pair objects
%       histogram:
% *       timestamp-start: (date-time)
% *       timestamp-stop: (date-time)
%         data: multi-dim array of time-series key-value pair objects
%       geo-histogram:
% *       timestamp-start: (date-time)
% *       timestamp-stop: (date-time)
%         data: multi-dim array of time-series key-value pair objects
%         geo-tiles: array of Geo-Tile objects (visited tiles)
%       general-purpose:
% *       timestamp: (date-time)
%         data: (depends on type of signal of the MC)
%       event-based:
%         timestamp: (date-time)
%         event-sample-strategy:
%           { real-time-event, trigger-event, threshold-event }
%         data: (Event Sample object)
%       basic-cpp-information:
% *       timestamp: (date-time)
%         data: (depends on type of signal of the MC)
%
% * above indicates related to a data restrictions condition
%
%   Event Sample object
%     timestamp: (date-time)
%     value: (string)
%     datapackages: optional array of Data Package
%
%   Timestamp
%     e.g. "2020-01-01 02:02:02"
%
