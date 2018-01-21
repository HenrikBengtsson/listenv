<%
## Reuse the package vignette
md <- R.rsp::rstring(file="vignettes/listenv.md.rsp", postprocess=FALSE)

## Drop everything before the first subheader ("H2")
md <- unlist(strsplit(md, split="\n", fixed=TRUE))
md <- md[-seq_len(grep("^## ", md)[1]-1)]

## Drop the footer
md <- md[seq_len(grep("^---", md)[1]-1)]

## Output
cat(md, sep="\n")
%>
