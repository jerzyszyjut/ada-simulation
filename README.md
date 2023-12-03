# ada-simulation
Simple computer store simulation written in Ada lang. The goal of the project was to get to know the rendezvous programming paradigm. 

To run the project you can either use [GNAT Studio](https://github.com/AdaCore/gnatstudio) or gnatmake which I prefer. 

In the project, we have the shop process which has storage for computer parts. Producer processes deliver different computer parts in random time intervals and the shop decides whether to accept them based on computer schemas and storage contents. Client processes visit shop in random time intervals requesting computers from random computer schemas. Only one computer for a client can be assembled at a time. The shop can randomly empose an embargo on certain products for a given time. At this time shop does not accept any products from the producer.
