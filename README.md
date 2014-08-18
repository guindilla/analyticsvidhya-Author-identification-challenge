analyticsvidhya
===============

[Analytics Vidhya Author identification challenge](http://www.analyticsvidhya.com/blog/2014/08/author-identification-challenge/)

Details hereafter from the web page.

Problem statement:
------------------
Classify all the articles written by Tavish or Kunal on analyticvidhya.com by the author’s name.

What Data you need to use for training your model?
--------------------------------------------------
All the articles written by Tavish and Kunal before 7th July 2014 can be used to train the model. You can use the date of article publish, day of article publish, tags of article publish and the content of the article. The data needs to be scrapped out of the website and used on local server.

What Data you need to score your model?
---------------------------------------
All the articles (excluding this article) written by Tavish and Kunal after 6th July 2014 need to be scored using your model.

**Help:** You can take reference from [this](https://www.youtube.com/watch?v=j1V2McKbkLo) video to start this analysis

What is the evaluation metric?
------------------------------
Average mis-classification rate of both training and scoring will be taken as the evaluation metric. For example 5 out of 10 in scoring and 50 out of 50 in training were found to be correct classes in training and scoring respectively. The average mis-classification rate will be 0.5 * (5/10 + 0/50) = 0.25. Hence, your score is 75%. You need to build model which has high predictive power and also stable over populations.

End Notes:
----------
- The aim of this challenge is to foster analytical thinking in our reader’s mind and have some fun with practical machine learning / analytics challenges!
- We will give the winner of this challenge a chance to blog about his solution on Analytics Vidhya. Of course, he takes away all the visibility, which comes on the platform!
- Last but not the least, the entire story presented before is hypothetical. It was created with the sole aim to create this challenge. All our data is secure and darling Jenika understands that she can’t play around with Dad’s stuff in office!
