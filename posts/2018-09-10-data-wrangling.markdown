---
title: Data wrangling with Clojure
author: Alexandre Krispin
date: Mon 10 Sep 2018 11:21:29 PM JST
tags: data, clojure, group-by, parse
---

# Parsing data inside a JSON data structure

Let's say we did a GET request to a server and we got back data with
dates. If we want to use it to display a graph with the time in
x-axis, you'll probably have to parse it. And now let's say we have this data:

```
(def data
  (walk/keywordize-keys [{"name" "Alex" "age" 32 "updated" "2018-09-03T12:29:14+00:00"}
                         {"name" "Joe" "age" 26 "updated" "2018-09-02T14:45:38+00:00"}]))
```

If we want to transform `2018-09-02T14:45:38+00:00` as `2018-09-02`,
we would have to split our date at `T` and take the first part of the resulting expression:

```
(defn parse-date [raw-date]
  (first (str/split raw-date #"T")))
```

Then we can map our new function to each json object in our array with the `update-in` function to apply our parse-date only to dates.

```
(defn format-data
  [raw-data]
  (map #(update-in % [:updated] parse-date) raw-data))
```

The resulting data would be just as expected:

```
user> (format-data data)
({:name Alex, :age 32, :updated 2018-09-03} {:name Joe, :age 26, :updated 2018-09-02})
```

OK, great. But now we have an other kind of necessity, to group data
by dates. For that, let's say we have this kind of query as a new
data:

```
(def queries
  [{:url "www.google.com" :fetched "2018-09-02"}
   {:url "www.google.com" :fetched "2018-09-02"}
   {:url "www.yahoo.com" :fetched "2018-09-01"}
   {:url "www.facebook.com" :fetched "2018-09-01"}])
```

We want to get the views of each website per fetched date. Thanks to
the very useful function `group-by`, it is easy peasy to do:

```
user> (group-by :fetched queries))
{2018-09-02 [{:url www.google.com, :fetched 2018-09-02}
             {:url www.google.com, :fetched 2018-09-02}],
 2018-09-01 [{:url www.yahoo.com, :fetched 2018-09-01}
             {:url www.facebook.com, :fetched 2018-09-01}]}
```

It is a good starting point, but still we have redundant data now, and
we still don't know the number of views per day.

So what we want now is a function parsing our transformed data, get
for each url a count and associate it to a new hash map to easily
access it later on.

```
(defn get-urls
  [raw-data]
  (hash-map (first raw-data) (map :url (second raw-data))
            :count (count (map :url (second raw-data)))
            ))
```

That's it, now if we map our function `get-urls` to our previous
grouped queries, we will have the number of views per day:

```
user> (map get-urls grouped-queries)
{2018-09-02 (www.google.com www.google.com), :count 2}
{2018-09-01 (www.yahoo.com www.facebook.com), :count 2})
```

# Handling data with SQLite

What I like the most with Clojure is its simplicity for manipulating
data structures, any sort of data. Let's say we want to convert our
data as a sql database. Nothing more easier than that with Clojure.

We would create a pool to connect to our database with JDBC:

```
(def my-db {:classname "org.sqlite.JDBC"
              :subprotocol "sqlite"
              :subname "whatever_name.db"})
```

Then to query it, we would use `select`. If we want to insert data, we
would do it by indicating the right pool, the right table to write
into new data, and the said data.

```
(def sql-data
  (jdbc/query my-db (sql/select * :some-table)))

(defn insert-data
  [data]
  (jdbc/insert! my-db :some-table data))
```

Easy peasy, I like it much than the way things are done in Python.
