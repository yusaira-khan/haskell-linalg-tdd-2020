module KalmanFilter (KalmanFilter,KalmanFilter.init,update, adjust) where
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
update :: KalmanFilter -> Scalar -> KalmanFilter
update kal ix =
  let
   pr = r kal
   pq = q kal
   pp = p kal
   px = x kal
   tp = pp + pq
   tk = pp/(pp+pr)
   nx = px + tk * (ix -px)
   np = (1-tk)*tp
  in KalmanFilter{r= pr, q=pq, p=np, x=nx}

adjustLeft :: Scalar -> (KalmanFilter,Vector)-> (KalmanFilter,Vector)
adjustLeft l (pk,px )=
  let
   nk = update pk l
   nx = x nk
  in (nk,px++[nx])
adjustRight :: (KalmanFilter,Vector)-> Scalar -> (KalmanFilter,Vector)
adjustRight (pk,px) l =
  let
   nk = update pk l
   nx = x nk
  in (nk,nx:px)
adjust :: Scalar -> Scalar -> Vector  -> Vector
adjust iq ir d =
  let
    k = KalmanFilter.init iq ir
    ndlz = foldl adjustLeft (k,[]) d
    ndrz = foldr adjustRight (k,[]) d
    (_,l)  = snd ndlz
    (_,r) = snd ndrz
  in l

