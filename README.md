# Cross-domain Meta-learning for Time-series Forecasting

 There exist a plethora of algorithms that can be used for the time-series forecasting problem, ranging from simple methods like the Moving Average to sophisticated Machine Learning approaches like Neural Networks. In order to use most of these algorithms, a number of user-defined parameters need to be specified, leading to an exponential explosion of the space of potential solutions. Since the trial-and-error approach to finding a good algorithm for solving a given problem is typically intractable, researchers and practitioners need to resort to a more intelligent search strategy, with one option being to constraint the search space using past experience -- an approach known as Meta-learning. Although potentially attractive, Meta-learning comes with its own challenges. Gathering a sufficient number of Meta-examples, which in turn requires collecting and processing multiple datasets from each problem domain under consideration is perhaps the most prominent issue. In this paper, we are investigating in what situations the use of additional data can improve the performance of a Meta-learning system, with a focus on the cross-domain transfer of Meta-knowledge.

 In the end, similarity based cluster analysis of Meta-features has been performed to evaluate whether Meta-learning works for the mentioned investigations. There was not much room for improvement for Meta-learner but it made its place between the best possible Base-learner and best performing Base-learner (Moving Average) among three others.

This system is an implementation of 
<a href="https://www.sciencedirect.com/science/article/pii/S1877050918311785">Ali, A., Gabrys, B., and Budka, M. Cross-domain Meta-learning for Time-series Forecasting. In Procedia Computer Science, 126(1), pages 9-18, Elsevier, Sep 2018.</a>