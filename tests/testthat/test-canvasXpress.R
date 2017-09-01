context("canvasXpress BASE")

data1     <- data.frame(a = c(1:3), b = c("A", "B", "C"),
                        row.names = c("row1", "row2", "row3"))
data1.var <- data.frame(row1 = c(10:12), row2 = c(20:22), row3 = c(30:32))
data1.smp <- data.frame(a = c(30:32), b = c(40:42))


test_that("Missing Data", {
    expect_error(canvasXpress(data = NULL),  
                 regexpr =  "data cannot be NULL!")
})

test_that("Incorrect Data Type", {
    expect_error(canvasXpress(data = list(data1, data1.smp, data1.var)),  
                 regexpr =  "data must be a data.frame or matrix classed object")
    expect_error(canvasXpress(data = "Test"),  
                 regexpr =  "data must be a data.frame or matrix classed object")
})

test_that("Missing GraphType", {
    expect_error(canvasXpress(data = data1,
                              graphType = NULL),
                 regexpr = "graphType cannot be NULL!")
})

test_that("Incorrect GraphType", {
    expect_error(canvasXpress(data = data1,
                              graphType = ""),
                 regexpr = "graphType is invalid, must be one of <.*")
    expect_error(canvasXpress(data = data1,
                              graphType = "Scatter4d"),
                 regexpr = "graphType is invalid, must be one of <.*")
})

test_that("Name mismatches", {
    expect_error(canvasXpress(data = data1,
                              smpAnnot = data1.var),
                 regexpr = "Rownames in smpAnnot are different from column names in data")
    expect_silent(canvasXpress(data = data1,
                               smpAnnot = data1.smp))
    
    expect_error(canvasXpress(data = data1,
                              varAnnot = data1.smp),
                 regexpr = "Column names in varAnnot are different from row names in data")
    expect_silent(canvasXpress(data = data1,
                               varAnnot = data1.var))
})


# test_that("Network Graph Input", {
#     ndata <- matrix(1:10, nrow = 2, dimnames = list(c("row1", "row2"), c("id", "C2", "C3", "C4", "C5")))
#     edata <- matrix(1:10, nrow = 2, dimnames = list(c("row1", "row2"), c("id1", "id2", "C3", "C4", "C5")))
# 
#     expect_silent(canvasXpress(graphType = "Network",
#                                nodeData = ndata, edgeData = edata))
#     
#     n.df <- as.data.frame(ndata)
#     e.df <- as.data.frame(edata)
#     l    <- list(col1 = c(1:20), col2 = c("fred", "barney", "wilma"))
#     bad  <- data.frame(oops = c(1, 2), bad = c("fred", "barney"))
#     bad2 <- data.frame(id1 = c(1, 2),  bad = c("fred", "barney"))
# 
#     expect_error(canvasXpress(graphType = "Network",
#                               nodeData = l, edgeData = e.df),
#                  regexpr =  "nodeData must be a data frame or a matrix class object")
#     expect_error(canvasXpress(graphType = "Network",
#                               nodeData = n.df, edgeData = l),
#                  regexpr =  "edgeData must be a data frame or a matrix class object")
#     
#     expect_error(canvasXpress(graphType = "Network",
#                               nodeData = bad, edgeData = edata),
#                  regexpr =  "missing 'id' header in nodeData dataframe")
#     expect_error(canvasXpress(graphType = "Network",
#                               nodeData = ndata, edgeData = bad),
#                  regexpr =  "missing 'id1' header in edgeData dataframe")
#     
#     expect_error(canvasXpress(graphType = "Network",
#                               nodeData = ndata, edgeData = bad2),
#                  regexpr =  "missing 'id2' header in edgeData dataframe")
# 
#     expect_silent(canvasXpress(graphType = "Network",
#                                nodeData = n.df, edgeData = edata))
#     expect_silent(canvasXpress(graphType = "Network",
#                                nodeData = ndata, edgeData = e.df))
#     expect_silent(canvasXpress(graphType = "Network",
#                                nodeData = n.df, edgeData = e.df))
# })
# 
# test_that("Venn Graph Input", {
#     data <- data.frame(A = 57, B = 12, C = 67, D = 72, AB = 4, AC = 67,
#                        AD = 25, BC = 67, BD = 27, CD = 38, ABC = 69, 
#                        ABD = 28, ACD = 52, BCD = 46, ABCD = 3)
#     data.l <- list(col1 = c(1:20), col2 = c("fred", "barney", "wilma"))
#     legend <- list(A = "List1", B = "List2", C = "List3", D = "List4")
#     
#     expect_silent(canvasXpress(graphType = "Venn",
#                                vennData = data, vennLegend = legend))
#     
#     expect_error(canvasXpress(graphType = "Venn", 
#                               vennData = data.l, vennLegend = legend),
#                  regexpr =  "vennData must be a data frame or a matrix class object.")
# })
# 
# test_that("Genome Graph Input", {
#     expect_error(canvasXpress(graphType = "Genome",
#                               genomeData = c(1, 2, 3)),
#                  regexpr =  "Not implemented yet!")
# })
# 
# 



# Shiny Functionality
test_that("Shiny UI Object Creation", {
    obj <- suppressWarnings(canvasXpressOutput("testID"))

    expect_s3_class(obj, "shiny.tag.list")
    expect_match(obj[[1]]$name,  "div")
    expect_match(obj[[1]]$attribs$id,    "testID")
    expect_match(obj[[1]]$attribs$class, "canvasXpress html-widget html-widget-output")
    expect_match(obj[[1]]$attribs$style, "width:100%; height:400px;")
})

test_that("Shiny Render", {
    expect_s3_class(renderCanvasXpress(NULL), "shiny.render.function")
})

test_that("Shiny Examples", {
    expect_error(cxShinyExample("badexample"),
                 regexpr = "Valid examples are: 'example1', 'example2', 'example3'")
    expect_message(cxShinyExample(NULL),
                 regexpr = "Valid examples are: 'example1', 'example2', 'example3'")
})

