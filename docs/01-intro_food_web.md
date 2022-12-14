# (PART\*) PROJECT 2 {-}

# Introduction to networks and food webs {#intro-foodweb}



You might remember from high school hearing about the food chain: that herbivores eat
plants, carnivores eat those herbivores, and so on in a successive chain of food relationships. **Relationships** can be represented as _data_ and analysed using _algorithms_.

The food web is an extension of this concept. Because there exists many species of plants, herbivores, and carnivores in an ecosystem, there exists many interconnected food chains. The food web represents those interconnected food chains at the scale of an ecosystem and it can be mapped as a network of interactions between species.

Ecological networks, such as food webs, can be represented in mathematics as a matrix, converted to a graph, and analysed using algorithms developed for [**network**](https://en.wikipedia.org/wiki/Network_theory) and [**graph theories**](https://en.wikipedia.org/wiki/Graph_theory).

We can imagine food webs as **social media networks**, but instead of representing relationships between friends, we have relationships between **enemies**.

<center>

![The first representation of a food web (Camerano, 1880) \label{camerano_network}](images/01-intro_food_web/camerano_network.png){width=500px}

</center>


## What are networks?

Networks are graphical objects that represents entities (e.g. people) as nodes, and the connections between these nodes as edges (or links).

Similarly, pairwise relationships in biological and ecological systems can be represented as nodes and links:
- the nodes of the network represent 'species',
- the links represent 'relationships' between those species.

In food webs, the links represents 'who eats whom' and points to the predator.
In the illustration below, each node (or species) is represented by a circle at the exception of the top predator—the Atlantic cod, _Gadus morhua_; each link is represented by an arrow pointing from the prey to the predator.

<center>

![Graphical representation of a food web \label{foodweb-topology}](images/01-intro_food_web/food-web-topology.png){width=500px}

</center>

## How to analyse network data

We can analyse networks according to two schools: topological analysis and flow analysis.

Topological analysis focuses on the structure of the network:
- which are the nodes,
- which other nodes are they connected to,
- on average, how many relationships are represented in the network,
- how are those relationships distributed across nodes,
- are there any social subgroups signaling communities,
- ...



For a longer intro to food webs and why we study food webs, you can read the summary of my [**doctoral thesis**](https://www.doria.fi/bitstream/handle/10024/184805/olivier_pierre.pdf?sequence=2&isAllowed=y).

In the next section, we will build network graphs from their matrix forms, analyse them, and use methods from statistics and machine learning to perform various data science tasks.
