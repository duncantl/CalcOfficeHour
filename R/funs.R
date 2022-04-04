getDoc = 
function(id, con, term = "202203", verbose = FALSE) {
        if(verbose)
            message(id)
        u = sprintf("https://sisr.ucdavis.edu/secure/profiles/CampusStudentProfile.cfm?sid=%s", id)

        ##        txt = getURLContent(u, curl = con)
        txt = postForm(u, termCode = term, style = "post", curl = con)        
        doc = htmlParse(txt)
        tbl = getNodeSet(doc, "//table[@id = 'CoursesDisplayTable']")
    # readHTMLTable(tbl[[1]], stringsAsFactors = FALSE)
    # don't use readHTMLTable
        myReadTable(tbl[[1]])  
}

myReadTable =
function(node)
{
      # Get the names of the columns/variables
   vars = xpathSApply(node[[1]], "./td",  xmlValue)

      # For each row, get the values
   rows = lapply(node[-1], function(x) as.data.frame(xpathApply(x, "./td",  xmlValue), stringsAsFactors = FALSE))
      # Discard the rows which have no values
   keep = sapply(rows, ncol) > 0
   rows = rows[keep]

      # Now figure out the rowspan values for each column in each row. This could be 1, 2 and 3 (maybe more)
#   rs = sapply(node[-1][keep], function(r) which(sapply(r[names(r) == "td"], xmlGetAttr,  "rowspan",  0) > 1))
   rspans = sapply(node[-1][keep], function(r) sapply(r[names(r) == "td"], xmlGetAttr,  "rowspan",  0, as.integer))
      # get the columns in each row that span more than 1 row.
   crs = sapply(rspans, function(x) which(x > 1))
      # which rows have any columns that span more than one row.
   w = which(sapply(crs,  length) > 0)
   nrs = sapply(rspans[w], max) - 1L
   ir = rep(w, nrs)
   dups = rows[ir]
   # subrows w + 1L
   subrows = unlist(mapply(function(s, n) s + seq_len(n), w,  nrs))

   dups = mapply(function(r1, r2, cols) {
       r1[setdiff(seq(along = r1), cols)] = r2
       r1
   }, dups, rows[subrows], crs[ir],  SIMPLIFY = FALSE)
          
   rows[subrows] = dups

    # Put the names on each data.frame
   rows = lapply(rows, function(x) structure(x, names = vars))
    # stack them together.
   do.call(rbind, rows)
   
#    readHTMLTable(tbl[[1]], stringsAsFactors = FALSE)    
}


procStudentTimes =
function(tb, id)    
{
    tm = tb[["Days/Times"]]
    tms = tm[!is.na(tm)]
    tms = gsub("Â ", "",  unlist(tms))
    tms = tms[tms != ""]

    d = mkDayTimes(tms)
    d$sid = id
    d
}

mkDayTimes =
function(tms)
{    
    dy = gsub(" .*", "", tms)
    tm = gsub("^[^ ]+ ", "", tms)

 
    d = strsplit(dy, "")
    nd = sapply(d, length)
    send = strsplit(tm, " - ", fixed = TRUE)
    data.frame(day = unlist(d),
               start = rep(sapply(send, `[`, 1), nd),
               end = rep(sapply(send, `[`, 2), nd),            
               stringsAsFactors = FALSE)    
}

cvtTime =
function(tm)
{
#    p = grepl(tm, "PM$")
    tm = strptime(tm, "%I:%M%p")
    tm$hour*60 + tm$min
}



if(FALSE) {
plotTimes =
function(x, breaks = seq(0, max(x) + 1, by = 10),  ncolors = length(breaks) - 1)
    # allow specifying breaks or ncolors.
{
    bb = seq(0, 255, length = ncolors)/255
    image(t(x[, ncol(x):1])[,nrow(x):1], breaks = breaks, col = rev(mapply(rgb, bb, bb, bb)), axes = FALSE)
    box()


    axis(1, at = seq(0, 1, length = ncol(x)), labels = colnames(x))
    axis(2, at = seq(0, 1, length = nrow(x)), labels = rownames(x))

    text(rep(seq(0, 1, length = ncol(x)), each = nrow(x)),
         rep(seq(0, 1, length= nrow(x)), nrow(x)),
         as.character(x), col = "red")
}
} # end if(FALSE)


plotTimes =
function(x, breaks = seq(0, max(x) + 10, by = 10), ncolors = length(breaks) - 1,
         main = Sys.Date(),  ...)
    # allow specifying breaks or ncolors.
{
#    plot(type = "n")

    X0 = seq(0, 1, length = ncol(x))
    Y0 = seq(0, 1, length= nrow(x))
    X = rep(X0, each = nrow(x))
    Y = rep(Y0, nrow(x))

    #nx = t(x[, ncol(x):1])[,nrow(x):1]
#    nx = matrix(x, ncol(x), nrow(x))[ncol(x):1,]

# browser()
    
    nx = t(x) # matrix(x, ncol(x), nrow(x))[ncol(x):1,]
    bb = rev(seq(0, 255, length = ncolors)/255)
    image(X0, Y0, z = nx, breaks = breaks, col = mapply(rgb, bb, bb, bb), axes = FALSE, xlab = "Day of Week", ylab = "Hour of Day", ...)
    box()
    axis(1, at = seq(0, 1, length= ncol(x)), labels = colnames(x))

    # Draw lines.
    ry = par()$usr[3:4]

#    y = seq(ry[1], ry[2], length= nrow(x))    
    y = seq(ry[1], ry[2], by = max(diff(Y)))
    axis(2, at = y[-length(y)], labels = rownames(x))
    abline(h = y)
    text(X, Y, as.character(x), col = "red")
}


plot.UnavailableTimes =
function(x, y, hours = 17:37, ...)
   plotTimes(x[hours,], ...)
