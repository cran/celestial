hms2deg <-
function(h,m,s){
if(any(h<0 | h>24)){stop('All h values should be 0<=h<=24')}
if(any(m<0 | m>=60)){stop('All m values should be 0<=m<60')}
if(any(s<0 | s>=60)){stop('All s values should be 0<=s<60')}
    if(is.matrix(h) || is.data.frame(h)){
		if(ncol(h) == 1){h=h[,1]}
        else	if(ncol(h) == 2){m = h[, 2];h = h[, 1]}
        else	if(ncol(h) == 3){s = h[, 3];m = h[, 2];h = h[, 1]}
        }
    H=floor(as.numeric(h))
    M=floor(as.numeric(m))
    S=as.numeric(s)
    totaldeg=(H*15) + (M*15/60) + (S*15/3600)
    return=totaldeg
}
