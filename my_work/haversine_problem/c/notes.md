# Latency vs. Throughput

Let's talk about Latency and Throughput in the context of Laundry machines

Let's say we have two configuration of Laundry machine setup

Setup A:
    - 1 Washing Machine that takes 1 hour to do a load
    - 1 Drying Machine that also takes 1 hour to do a load

Setup B:
    - 1 Washing+Drying machine that does both (washes the clothes, drains the water and dries the cloths). This takes 2 hours overall to do a load.

Assume load to be same across each setup and machine (let's say 5 kg of clothes)

Let's say we have to wash a single load
Setup A:
|Hour  |Machine 1 |Machine 2|
| ---- | -------- | ------- |
|1     |Load1 (W) |x        |
|2     |x         |Load1 (D)|
Setup B:
|Hour  |Machine 1 |
| ---- | -------- |
|1     |Load1 (W) |
|2     |Load1 (D) |
In both cases, it takes two hours to do a single load. This is `latency`. `Latency` is defined as the *amount of time it takes do a unit operation* (*op/time*)

However, let's say we have to wash 3 loads of clothes
Setup A:
|Hour  |Machine 1 |Machine 2|
| ---- | -------- | ------- |
|1     |Load1 (W) |x        |
|2     |Load2 (W) |Load1 (D)|
|3     |Load3 (W) |Load2 (D)|
|4     |x         |Load3 (D)|
It takes 4 hours to do in Setup A.
Setup B:
|Hour  |Machine 1 |
| ---- | -------- |
|1     |Load1 (W) |
|2     |Load1 (D) |
|3     |Load2 (W) |
|4     |Load2 (D) |
|5     |Load3 (W) |
|6     |Load3 (D) |
It takes 6 hours to do in Setup B. This is `throughput`. `Throughput` is defined as the *amount of operations it can do in a unit time* (*ops/time*)

So it is cleary can be seen that Setup A is much better than Setup B, even though the latency of both the setups are same. The throughput is what differentiates Setup A to be favourable. If you have lot of clothes to launder, then Setup A is much better. At homes, people usually prefer Setup B though, because it eliminates the need for manually transfer the load from one machine to the other. It is very important to note that which setup is better depends how much load we are going to put.

Note that in the community people usually talk about the reciprocal of the throughput to make the units aligned between latency and "reciprocal" throughput. Also they might might drop "reciprocal" and just call it throughput, so don't get confused by the terms. Understand what they mean. One is talking about single load (latency), and the other is talking about multiple loads (throughput).


# Intel V-Tune Download Link:

Linux: https://www.intel.com/content/www/us/en/developer/tools/oneapi/base-toolkit-download.html?packages=oneapi-toolkit&oneapi-toolkit-os=linux&oneapi-lin=offline
