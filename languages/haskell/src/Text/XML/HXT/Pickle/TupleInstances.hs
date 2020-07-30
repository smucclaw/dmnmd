module Text.XML.HXT.Pickle.TupleInstances where
import Text.XML.HXT.Core

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f) => XmlPickler (a, b, c, d, e, f) where
  xpickle = xp6Tuple xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g) => XmlPickler (a, b, c, d, e, f, g) where
  xpickle = xp7Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h) => XmlPickler (a, b, c, d, e, f, g, h) where
  xpickle = xp8Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i) => XmlPickler (a, b, c, d, e, f, g, h, i) where
  xpickle = xp9Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j) => XmlPickler (a, b, c, d, e, f, g, h, i, j) where
  xpickle = xp10Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k) where
  xpickle = xp11Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l) where
  xpickle = xp12Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  xpickle = xp13Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m, XmlPickler n) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  xpickle = xp14Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m, XmlPickler n, XmlPickler o) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  xpickle = xp15Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m, XmlPickler n, XmlPickler o, XmlPickler p) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  xpickle = xp16Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m, XmlPickler n, XmlPickler o, XmlPickler p, XmlPickler q) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where
  xpickle = xp17Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m, XmlPickler n, XmlPickler o, XmlPickler p, XmlPickler q, XmlPickler r) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where
  xpickle = xp18Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m, XmlPickler n, XmlPickler o, XmlPickler p, XmlPickler q, XmlPickler r, XmlPickler s) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where
  xpickle = xp19Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m, XmlPickler n, XmlPickler o, XmlPickler p, XmlPickler q, XmlPickler r, XmlPickler s, XmlPickler t) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where
  xpickle = xp20Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m, XmlPickler n, XmlPickler o, XmlPickler p, XmlPickler q, XmlPickler r, XmlPickler s, XmlPickler t, XmlPickler u) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) where
  xpickle = xp21Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m, XmlPickler n, XmlPickler o, XmlPickler p, XmlPickler q, XmlPickler r, XmlPickler s, XmlPickler t, XmlPickler u, XmlPickler v) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) where
  xpickle = xp22Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m, XmlPickler n, XmlPickler o, XmlPickler p, XmlPickler q, XmlPickler r, XmlPickler s, XmlPickler t, XmlPickler u, XmlPickler v, XmlPickler w) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) where
  xpickle = xp23Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle

instance (XmlPickler a, XmlPickler b, XmlPickler c, XmlPickler d, XmlPickler e, XmlPickler f, XmlPickler g, XmlPickler h, XmlPickler i, XmlPickler j, XmlPickler k, XmlPickler l, XmlPickler m, XmlPickler n, XmlPickler o, XmlPickler p, XmlPickler q, XmlPickler r, XmlPickler s, XmlPickler t, XmlPickler u, XmlPickler v, XmlPickler w, XmlPickler x) => XmlPickler (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) where
  xpickle = xp24Tuple xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle xpickle