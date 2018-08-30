

maturities<-sort(unique(spx_vol$Days))
strikes<-seq(from=1000,to=3000,length.out=50)
vol_df<-spx_vol[Date==min(Date)]


strikes_in<-sort(unique(vol_df$Strike))
maturities_in<-sort(unique(vol_df$Days))
j<-data.table::frank(vol_df$Strike,ties.method = "dense")
i<-data.table::frank(vol_df$Days,ties.method = "dense")
vol_grid<-t(apply(sparseMatrix(
  j=j,
  i=i,
  x=vol_df$ImpliedVol,
  use.last.ij=TRUE
),1,function(a){
  ndx<-which(a>0)
  approx(x=ndx,y=a[ndx],xout=seq_along(a),yleft=max(a[ndx]),yright=min(a[ndx]))$y
}))

res<-bilinear(
    y=strikes_in,
    x=maturities_in,
    z=vol_grid,
    y0=rep(strikes,times=length(maturities)),
    x0=rep(maturities,each=length(strikes))
)
data.table(Strike=res$y,Days=res$x,ImpliedVol=res$z)
  
  

