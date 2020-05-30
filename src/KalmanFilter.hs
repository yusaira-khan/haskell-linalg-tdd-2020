module KalmanFilter (KalmanFilter) where
import Vector
data KalmanFilter = KalmanFilter
{  r::Scalar -- R
 , p::Scalar -- P
 , q::Scalar -- Q
 , x::Scalar -- X
 , k::Scalar -- K
}
init :: KalmanFilter
init = undefined
