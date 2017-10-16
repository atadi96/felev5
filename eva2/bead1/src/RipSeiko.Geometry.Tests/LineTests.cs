using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using RipSeiko.Geometry;

namespace RipSeiko.Geometry.Tests
{
    [TestClass]
    public class LineFTests
    {
        private static bool LineFIsOkay(LineF l) => l.P1.X <= l.P2.X && l.P1.Y <= l.P2.Y;

        [TestMethod]
        public void LineFConstructor()
        {
            var vertical = new PointF(0,1);
            var horizontal = new PointF(1,0);
            var line1 = new LineF(vertical, horizontal);
            var line2 = new LineF(horizontal, vertical);

            Assert.AreEqual(line1.P1, vertical);
            Assert.AreEqual(line1.P2, horizontal);

            Assert.AreEqual(line2.P1, horizontal);
            Assert.AreEqual(line2.P2, vertical);
        }

        [TestMethod]
        public void LineFIntersect()
        {
            var vertical = new PointF(0, 1);
            var horizontal = new PointF(1, 0);
            var line1 = new LineF(vertical, horizontal);
            var line2 = new LineF(horizontal, vertical);
            var line3 = new LineF(PointF.Zero, new PointF(1, 1));

            Assert.AreEqual(
                Geometry.IntersectionType.None,
                Geometry.Intersect(
                    new LineF(new PointF(1, 1), new PointF(10, 1)),
                    new LineF(new PointF(2, 8), new PointF(5, 7))
                )
            );
            Assert.AreEqual(Geometry.IntersectionType.Proper, Geometry.Intersect(line2, line3));
            Assert.AreEqual(Geometry.IntersectionType.Proper, Geometry.Intersect(line3, line2));
            Assert.AreEqual(Geometry.IntersectionType.Infinite, Geometry.Intersect(line1, line2));
            Assert.AreEqual(Geometry.IntersectionType.Infinite, Geometry.Intersect(line2, line1));
            Assert.AreEqual(Geometry.IntersectionType.Infinite, Geometry.Intersect(line1, line1));
            Assert.AreEqual(Geometry.IntersectionType.Infinite, Geometry.Intersect(line1, line1));
            Assert.AreEqual(Geometry.IntersectionType.None, Geometry.Intersect(line1, line2 + new Vector2F(1, 0)));
            Assert.AreEqual(Geometry.IntersectionType.None, Geometry.Intersect(line1, line2 + new Vector2F(0, 1)));
            Assert.AreEqual(Geometry.IntersectionType.None, Geometry.Intersect(line1, line2 + new Vector2F(-1, 0)));
            Assert.AreEqual(Geometry.IntersectionType.None, Geometry.Intersect(line1, line2 + new Vector2F(0, -1)));
            Assert.AreEqual(
                Geometry.IntersectionType.Proper,
                Geometry.Intersect(
                    new LineF(PointF.Zero, new PointF(0,2)),
                    new LineF(new PointF(-1, 1), new PointF(1, 1))
                )
            );/*
            Assert.AreEqual(
                Geometry.IntersectionType.Improper,
                Geometry.Intersect(
                    new LineF(PointF.Zero, new PointF(0, 1)),
                    new LineF(new PointF(-1, 1), new PointF(1, 1))
                )
            );*/
            Assert.AreEqual(
                Geometry.IntersectionType.Proper,
                Geometry.Intersect(
                    new LineF(new PointF(1, 1), new PointF(10, 1)),
                    new LineF(new PointF(2, 8), new PointF(5, 0))
                )
            );
        }
    }
}
