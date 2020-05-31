module KalmanFilter (KalmanFilter) where
import Vector
data KalmanFilter = KalmanFilter
{  r::Scalar -- R
 , p::Scalar -- P
 , q::Scalar -- Q
 , x::Scalar -- X
}
init :: KalmanFilter
init iq ir = KalmanFilter {
  q=iq,
  r=ir,
  p=0,
  x=0
  }
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
  

