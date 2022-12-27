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

### Not much time

This is my weekend project, literally done in a weekend. As I have mainly weekends to work over here, it may take some
substantial time for this project to come to fruition or to any conclusion.

## The idea

### Block Bloom filter

Block Bloom filter select one or more blocks (usually, cache lines or SIMD vectors) according to one set of hashes and then selects bits inside according to the other sets of hashes.

Typical Block Bloom filter has two level structure and, if you think about it, represents a relation: for a pair (a,b) first level can be selected from the value of a and structure of second level can be selected from the value of b. Having defined filtering relation for two levels, we can go higher, defining it for an arbitrary number of levels, representing n-ary relations.

### ROBDD representation of a Bloom filter


