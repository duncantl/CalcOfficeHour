# Originally from ~/Classes/Davis/STA141B/OfficeHours/Schedule/code.R
mkTimes =
    # con is to SISR
function(course, department = "GSTA", ros = getRoster(course, yrquarter = term, department, ..., curl = con),
         con, term = "202203", verbose = FALSE,
         studentTimes = getStudentTimes(ros, con, term, verbose), ...)
{
    nstudents = length(unique(studentTimes$sid))
    st = studentTimes
    mkPairs(st, nstudents)
}



getStudentTimes =
function(ros, con, term = "202203", verbose = FALSE)
{
    tbls = lapply(ros$SID, function(id) try(getDoc(id, con, term, verbose)))
    names(tbls) = ros$SID
    tmp = mapply(procStudentTimes, tbls, ros$SID, SIMPLIFY = FALSE)

    st = do.call(rbind, tmp)
    # break into half hour increments
    # t1 and t2 are the start and end buckets that range from 0 -  47 meaning
    # the 30 minute intervals through the data.
    st$t1 = floor(cvtTime(st$start)/30)
    st$t2 = ceiling(cvtTime(st$end)/30)

    # Just making this an ordered factor but assigning to a different variable.
    st$day2 = ordered(st$day, levels = c("M", "T", "W",  "R",  "F"))

    st
}


mkPairs =
function(st, numStudents)
{
    HH = as.character(t(outer(0:23, c("00", "30"), paste,  sep = ":")))
    names(HH) = 0:47

#    tt = with(st, table(ordered(HH[as.character(t1)], levels = HH), day2))


    # Now fill in all the buckets for each student for each course.
    TT = matrix(0L, 48, 5, dimnames = list(HH, levels(st$day2)))

    for(i in 1:nrow(st)) {
        r = seq.int(st$t1[i], st$t2[i])
        TT[r, as.character(st$day2[i]) ] = TT[r, as.character(st$day2[i]) ] + 1L
    }
 
    # Column-wise get the totals of pairs of buckets.
    pairs = apply(TT, 2, function(x) x + c(0, x[-length(x)]))
    class(pairs) = "UnavailableTimes"
    attr(pairs, "numStudents") = numStudents
    
    pairs
}
