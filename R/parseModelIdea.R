#
# parse(text = "dnorm(mu, sd)T(0, )")
#
# mmm <- r'(model{
#
# for (i in 1:100) {
# mu[i] ~ dnorm(0, 1)
# sd[i] ~ dnorm(0, 1)
# T(0, )
# }
# for (i in 1:100) {
# contNormal[i] ~ dnorm(mu[i], sd[i])
# }
#
# })'
# cat(mmm)
# mmm2 <- mmm
#
# r <- gregexpr("~([^~]*)\\s*T(.*)", mmm)
# match <- regmatches(mmm, r)
#
# mmm2 <- gsub("~([^~]*)\\s*T([^)]*)", "~ truncated(\\1, \\2)", x = mmm)
# cat(mmm2)
#
# fn <- parse(text = 'for (i in 1:100) {
# mu[i] ~ dnorm(0, 1)
# logit(sd[i]) ~ truncated( dnorm(0, 1)
# , 0, )
# }
# for (i in 1:100) {
# contNormal[i] ~ dnorm(mu[i], sd[i])
# }')
#
# fn[[1]][[4]][[2]][[1]]
#
#
# expr <- fn[[1]][[4]][[3]]
#
#
# # This is a bad idea! try to use some package because there can be nested indexing and all that madness...
# # one problem here is also that there is no distinction between a variable and a function? NO: functions only appear as first element
# parseModel <- function(expr) {
#
# }
#
# isNode <- function(x) {
#   is.name(x) && as.character(x) %in% c("~", "<-", "=")
# }
# isTransformation <- function(x) {
#   is.name(x) && as.character(x) %in% c("logit") # TODO
# }
# isIndexing <- function(x) {
#   is.name(x) && as.character(x) %in% c("[") # TODO
# }
# isTruncated <- function(x) {
#   is.name(x) && as.character(x) == "truncated"
# }
#
# nodeParser <- function(expr) {
#   # this should unflatten everything inside of it
#   c(list(relation       = as.character(expr[[1L]]), # should be a type?
#          expr            = expr),
#     parseLhsNode(expr[[2L]]),
#     parseRhsNode(expr[[3L]])
#   )
#   indexNodes     = ,
#   rhsNodes       = ,
#   rhsExpression  = expr[[3]],
#   prior          = NA
# }
#
# expr0 <- expr
# # expr <- expr0
# expr <- expr[[-1]]
#
# parseLhsNode <- function(expr) {
#   if (isTransformation(expr[[1L]])) {
#     return(c(list(transformation = as.character(expr[[1L]])), parseLhsNode(expr[[-1L]])))
#   } else if (isIndexing(expr[[1L]])) {
#     return(list(indexing = as.character(expr[[1L]]), name =  expr[[2L]], dimension = length(expr[[3L]]), expr = expr[[3L]]))
#   } else {
#     return(list(name = expr[[1L]]))
#   }
# }
#
# parseLhsNode(expr)
# parseLhsNode(expr0)
#
# # exprR0 <- expr
# expr <- exprR0[[3]]
# expr <- expr[[2]]
# parseRhsNode <- function(expr) {
#   if (isTruncated(expr[[1L]])) {
#     return(c(list(truncated = TRUE, lowerbound = expr[[3]], upperbound = expr[[4]]), parseRhsNode(expr[[2L]])))
#   } else {
#     args <- vector("list", length(expr) - 1L)
#     for (i in seq_along(args))
#       args[[i]] <- expr[[i+1L]]
#     return(list(prior = expr[[1L]], args = args))
#   }
# }
#
# parseRhsNode(exprR0[[3]])
#
#
