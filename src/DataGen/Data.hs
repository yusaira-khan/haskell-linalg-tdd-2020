module DataGen.Data(samples,DataGen.Kalman.useGivenKalman) where
import qualified DataGen.DemoFileSamples
import qualified DataGen.DemoPlottingSamples
import qualified DataGen.Small
import qualified DataGen.Kalman
samples:: [Double]
samples = DataGen.Small.samples
