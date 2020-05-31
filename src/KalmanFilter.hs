module KalmanFilter (KalmanFilter,setFilterAndAdjustData) where
import Vector
data KalmanFilter = KalmanFilter {  r::Scalar -- R
 , p::Scalar -- P
 , q::Scalar -- Q
 , x::Scalar -- X
}
init :: Scalar -> Scalar-> KalmanFilter
init iq ir = KalmanFilter {
  q=iq,
  r=ir,
  p=0,
  x=0
  }
set ip ix k = KalmanFilter{
  q = q k,
  r = r k,
  p = ip,
  x = ix
}
update :: KalmanFilter -> Scalar -> KalmanFilter
update kal ix =
  let
   pr = r kal
   pq = q kal
   pp = p kal
   px = x kal
   tp = pp + pq
   tk = tp/(tp+pr)
   nx = px + tk * (ix -px)
   np = (1-tk)*tp
  in KalmanFilter{r= pr, q=pq, p=np, x=nx}

adjustLeft :: (KalmanFilter,Vector) -> Scalar -> (KalmanFilter,Vector)
adjustLeft (pk,px ) l =
  let
   nk = update pk l
   nx = x nk
  in (nk,px++[nx])
adjustRight :: Scalar -> (KalmanFilter,Vector) -> (KalmanFilter,Vector)
adjustRight l (pk,px)  =
  let
   nk = update pk l
   nx = x nk
  in (nk,nx:px)
adjustData :: KalmanFilter -> Vector  -> Vector
adjustData k d =
  let
    e = [] :: Vector
    ndlz = (foldl adjustLeft (k,e) d) :: (KalmanFilter,Vector)
    ndrz = (foldr adjustRight (k,e) d) :: (KalmanFilter,Vector)
    l  = snd ndlz
    r = snd ndrz
  in l


setFilterAndAdjustData :: Scalar -> Scalar -> Scalar -> Vector -> Vector
setFilterAndAdjustData ip iq ir d =
  let
    k = KalmanFilter.init iq ir
    k2= set ip (head d) k
  in adjustData k2 d
