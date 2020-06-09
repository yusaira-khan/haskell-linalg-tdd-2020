module DataGen.Kalman(useGivenKalman) where

import KalmanFilter

useGivenKalman = KalmanFilter.setFilterAndAdjustData 0.1 0.1 0.1
