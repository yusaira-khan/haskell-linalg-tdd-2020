module DataGen.Kalman(givenKalman) where

import KalmanFilter

givenKalman = KalmanFilter.setFilterAndAdjustData 0.1 0.1 0.1
