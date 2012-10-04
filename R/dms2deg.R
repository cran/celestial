dms2deg <-
function(d,m,s,sign='d'){
if(any(d< -90 | d>90) & sign[1]=='d'){stop('All d values should be -90<=d<=90')}
if(any(d< 0 | d>90) & sign[1]!='d'){stop('Since sign is specified, all d values should be 0<=d<=90')}
if(any(m<0 | m>=60)){stop('All m values should be 0<=m<60')}
if(any(s<0 | s>=60)){stop('All s values should be 0<=s<60')}

if(sign[1]=='d' & any(d==0)){stop('Some d values are 0, and these have ambiguous signs, specify sign explicitly')}
if(sign[1]=='d'){sign=sign(d);d=abs(d)}
if(sign[1]!='d' & length(sign) != length(d)){stop('d and sign lengths do not match')}

    if(is.matrix(d) || is.data.frame(d)){
		if(ncol(d) == 1){d=d[,1]}
        else	if(ncol(d) == 2){m = d[, 2];d = d[, 1]}
        else	if(ncol(d) == 3){s = d[, 3];m = d[, 2];d = d[, 1]}
    }
    D=floor(as.numeric(d))
    M=floor(as.numeric(m))
    S=as.numeric(s)
    totaldeg=(D)*sign + (M/60)*sign + (S/3600)*sign
    return=totaldeg
}
