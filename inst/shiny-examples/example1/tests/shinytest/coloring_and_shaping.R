app <- ShinyDriver$new("../../")
app$snapshotInit("coloring_and_shaping")

app$setInputs(colorBySel = "Petal.Width")
app$setInputs(shapeBySel = "Sepal.Width")
app$snapshot()
