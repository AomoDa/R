

function (jsonStr = NULL, file = NULL, array) 
{
    if (!is.null(jsonStr) & !is.null(file)) {
        stop("Must specify jsonStr OR file.")
    }
    if (is.null(file)) {
        MainList <- fromJSON(json_str = jsonStr)
    }
    if (is.null(jsonStr)) {
        MainList <- fromJSON(file = file)
    }
    ArrayList <- MainList[[array]]
    MainDF <- ldply(ArrayList, data.frame)
    return(MainDF)
}


# 需要rjson包和plyr包。
# 利用ladply将json转为data.frame
