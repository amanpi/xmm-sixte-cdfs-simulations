import logging
import numpy as np
from astropy import units as u
from astropy.coordinates import Angle, SkyCoord
from astropy.modeling.rotations import Rotation2D
import sys

ra = float(sys.argv[1])
de = float(sys.argv[2])
ro = float(sys.argv[3])

pointing = SkyCoord(ra, de, unit="deg")

rot = Rotation2D(-ro)

dra2, ddec2 = rot(-21.0, 67.0)

shifted_ra = (pointing.ra + dra2 * u.arcsec / np.cos(pointing.dec)).wrap_at(360 * u.deg)
shifted_dec = pointing.dec + ddec2 * u.arcsec

print(SkyCoord(shifted_ra, shifted_dec))

dra2, ddec2 = rot(5.85, -10.2)

shifted_ra = (pointing.ra + dra2 * u.arcsec / np.cos(pointing.dec)).wrap_at(360 * u.deg)
shifted_dec = pointing.dec + ddec2 * u.arcsec

print(SkyCoord(shifted_ra, shifted_dec))

dra2, ddec2 = rot(-62.8, -24.2)

shifted_ra = (pointing.ra + dra2 * u.arcsec / np.cos(pointing.dec)).wrap_at(360 * u.deg)
shifted_dec = pointing.dec + ddec2 * u.arcsec

print(SkyCoord(shifted_ra, shifted_dec))




