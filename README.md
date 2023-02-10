# Worst-case optimal joins using ROBDDs and (block) Bloom filters

## Description

Worst-case optimal (WCO) join algorithms do not produce more data than necessary (in big-O-notation sense).

For example, for a [triangles-from-a-set-of-nodes query they will not produce more than O(Nsqrt(N)) intermediate
results](https://justinjaffray.com/a-gentle-ish-introduction-to-worst-case-optimal-joins/), because that is what theoretically possible. Regular joins will produce O(N<sup>2</sup>) results.

Tyypical WCO joins use various succinct data structures to hold intermediate results. This may not go with
the data that cannot be stored succinctly efficiently, strings for one example.

The code here attempts to hold not more intermediate data that what WCO would allow (in big-O-notation sense) and be applicable
for different data types, including strings.

The big-O notation for WCO does not include dependence on the number of data scans performed, only dependence on the size of data,
namely, the size of data that is held internally. But we can perform constant number of scans or be dependent on a sublinear (log(N), log(1-err), etc) number of scans. This is what we try to achieve here: how to create a filter that would let us to filter data
during scans that will not overflow memoryy.

## Limitations

### Only equijoins for a moment

We work on equijoins - where WHERE clause contains equalities combined with the logical AND opoerator. The query we test algorithm
on is this "find all triangles" query:

```
CREATE TABLE g(f INT, t INT);
INSERT INTO g
VALUES
        (1, 2), (1, 3), (1, 4), (2, 4), (2, 5),
        (3, 4), (3, 6), (3, 7), (4, 5), (4, 7),
        (4, 8), (5, 8), (6, 7), (7, 8);
SELECT
    g1.f AS a, g1.t AS b, g2.t AS c
FROM
    g AS g1, g AS g2, g AS g3
WHERE
    g1.t = g2.f AND g2.t = g3.t AND g1.f = g3.f;
```

It may be possible to extend algorithm to non-quijoins, though. We can use hashing to buckets for ordering operations, etc.

### Not much time

This is my weekend project, literally done in a weekend. As I have mainly weekends to work over here, it may take some
substantial time for this project to come to fruition or to any conclusion.

## The idea

Let's say you have two tables with two integer fields and you want to find out rows that are row-wise equal. Let's assume that first table contains rows (1, 2) and (1,3) and second table contains rows (1, 2) and (1, 4).

We can assign variables to columns' values. Then the first table represents logic formula (f1=1 /\ f2=2) \/ (f1=1 /\ f2=2) - field 1 is equal to 1 AND field 2 is equal to 2 OR field 1 is equal to 1 and field 2 is equal to 2. The ssecond table represents formula (f1=1 /\ f2=2) \/ (f1=1 /\ f2=4).

If we logically AND these two formulas, we will get a resulting formula (f1=1 \/ f2=2).

The scan for rows that satisfy this resulting formula will result in rows that contains a pair (1, 2) in both tables.

This is all good and easy and well when number of values in rows is small. When it grows, we must fall back to hashing values and use of approximatins like Bloom filters.

### Block Bloom filter

Block Bloom filter select one or more blocks (usually, cache lines or SIMD vectors) according to one set of hashes and then selects bits inside blocks according to the other sets of hashes.

Typical Block Bloom filter has two level structure and, if you think about it, represents a relation: for a pair (a,b) first level can be selected from the value of a and structure of second level can be selected from the value of b. Having defined filtering relation for two levels, we can go higher, defining it for an arbitrary number of levels, representing n-ary relations.


### ROBDD representation of a Bloom filter

The example above has three equalities. We assigned to each equality it's own block index. ```g1.t``` and ```g2.f``` share block address for block 1, ```g2.t``` and ```g3.t``` share block address for block 2 and ```g1.t``` and ```g3.f``` share address for block 3. This can be easily extended to more equalities if needed.

Each record in ```g1``` generates a pair of block addresses, for block 1 and block 3. Same goes with other tables, which generate addresses for blocks 1 and 2 (```g2```) and blocks 2 and 3 (```g3```). These addresses are combined with logical OR into three ROBDDs which approximately represent complete relations in our three tables. After that, we combined them with logical AND to approximately represent intersections of all three relations modulo equalities between values.

Then we can reread data and for each pair of block addresses we can verify that they are belong to the final approximation. If the pair does not belong to neither of parts of final approximation, we reject this pair altogether. It cannot participate in any part of final result.

## Complexity of an algorithm

The algorithm has O(SN) complexity, where S is the sum of logarithms of the number of different values in either table and N is the maximum number of rows in any of the tables we scan.

Thus, we do not escape O(NlogN) of dynamic indices of SQLite or merging joins of Postgres.

Our only hope is to be in-memory especially dealing with strings or other relatively heavy data like Postgress arrays.

## Experimental results

### Grwoing number triangles

With 10000 nodes (14 bits per variable index) and:

* 80000 edges:
  - actual number of triangles is 471,
  - number of graph edges in triangles is 935,
  - individual pairs have BDD node counts of around 123800 (1.55x number of edges in graph),
  - resulting BDD has node count of 11947 (12.8x number of graph edges),
  - 1453 edges pass scan test, which is 518 edges more or 55% of additional overhead over correct number of edges,
  - resulting BDD contains 488 conjunctions.
* 90000 edges:
  - actual number of triangles is 702,
  - number of graph edges in triangles is 1394
  - individual pairs have BDD node counts of around 135900 (1.51x number of edges in graph),
  - resulting BDD has node count of 16336 (11.7x number of graph edges),
  - 2084 edges pass scan test, which is 690 edges more or 49% of additional overhead over correct number of edges,
  - resulting BDD contains 702 conjunctions.
* 100000 edges:
  - actual number of triangles is 966,
  - number of graph edges in triangles is 1911,
  - individual pairs have BDD node counts of around 147740 (1.48x number of edges in graph),
  - resulting BDD has node count of 21541 (11.3x number of graph edges),
  - 2855 edges pass scan test, which is 944 edges more or 49% of additional overhead over correct number of edges,
  - resulting BDD contains 966 conjunctions.

Results indicate that we have O(number of resulting edges) overhead for final ROBDD node count and number of edges passing the test. We will produce about 1.5^3 more data than is strictly required during final stage, if we choose to implement it in a nested loops style. That is O((strict number of edges to process)^3), which may qualify as WCO in some sense.

We need to store O(number of edges in graph) data for intermediate results. It might be possible to apply zero-supressed decision diagrams to store approximately O(number of nodes in graph) data for intermediate results, but I haven't studied that venue well yet.

Also, number of conjunctions (BDD represents a disjunctive normal form) in resulting ROBDD is equal, in our tests, to the number of actual triangles. It may not be always the case, because ROBDD contains unique relations and if we process the same set of edges twice or more times, we will get the same set of relations but many more triangles.

