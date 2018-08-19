# Explore Topics Prototype

A prototype UI combining a number of techniques to explore topics from within a corpus of short text examples. This leverages the tidyr tools available within R
to provide a simple workflow of

1. Loading Data

2. Exploring N Topics and the Top N Terms per topic.

3. Comparing the Top N Terms in each topic by their log odds ratio with other topics.

4. Manually assigning new labels to topics after review.

5. Allocate documents to topics based on the maximum probability for each topic weighted by the model.

6. Interactively enter new examples to review their topic assignments.

7. Train supervised classifier to label document examples against the generated topics. 

8. Interactively enter new examples to review the topics assigned by the supervised classifier.

## Loading Data

The data is expected in a CSV format containing headers. Documents are expected as short documents (rather than long textual passages). At least one column containing the text is expected. Optional columns are allowed for row identifiers and previously assigned labels.

![Example config ui](docs/images/load_data01.png)


## Exploring Topics.

Once the document is loaded, a topic model is generated using the LDA algorithm provided by the tidyr textmining tools. The user may elect the number of topics and can review the top terms for each topic, 10 topics at a time. To allow some sense of the highly ranked words that are associated with each topic. It is possible to enter a value for the number of words to include in the top terms list. This provides the opportunity to review the potential subject for each of the topics based on the most probable words for that group.

![Example top terms per topic](docs/images/explore_topics02.png)

## Review Log odds ratio

The log odds ratio of the top terms in a selected topic, versus the same terms in other topics, can give a sense as to whether a term is unique to a chosen topic versus its use in the other topics. Those terms having a log odds ratio close to 0 are common between the selected topic and the others. Whereas those having large magnitude values (either negative or positive) are unique largely to the selected topic. The user can compare from the perspective of a chosen topic and visually review the log odds ratio. The use of the this screen is intended to help the user decide which words are influential and distinct to a given topic. This can help to inform the choice of meanings to assign when manually relabelling topics. 

![Reviewing log odds ratios](docs/images/compare_logratio03.png)



