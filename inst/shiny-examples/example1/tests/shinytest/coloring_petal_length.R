app <- ShinyDriver$new("../../")
app$snapshotInit("coloring_petal_length")

app$setInputs(colorBySel = "Petal.Length")
app$snapshot()
