# Ideas:

* A package that looks for a best model based on a given dataset
    - A function that analyze a column from `data.frame`
        + Number of NA's
        + Coercable to `numeric`, `logial`
        + Number of distinct values
        + etc.
    - A function that tries different models based on the type of the columns (if the outcome vairable is `numeric` - `lm`, `glm`, etc.), compares these models, returns the best one.
    
* A pacakge that generates a simple yet extenssible folder structure for a research project.
    
* Load from Kaggle all datasets and winning models. Train a neural network to predict the best model for a given dataset.

* Participate in Kaggle competition.

* A function for `ggplot2` for cross-hatching of polygons -- `geom_hatched_polygon`:
    - define a сircumscribed square
    - define angeled lines in this square
    - find intersection of lines and shape edges, that will define the start and end of lines
    
* A test line