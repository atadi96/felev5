using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using RipSeiko.Geometry;

namespace RipSeiko.Geometry.Tests
{
    [TestClass]
    public class RectangleTests
    {
        [TestMethod]
        public void RectangleConstructor()
        {
            var rect = new Rectangle(new Point(2, 4), new Point(1, 2));

            Assert.AreEqual(rect.BottomLeft, new Point(1, 2));
            Assert.AreEqual(rect.TopLeft, new Point(1, 4));
            Assert.AreEqual(rect.BottomRight, new Point(2, 2));
            Assert.AreEqual(rect.TopRight, new Point(2, 4));

            Assert.AreEqual(rect.Size, new Size(1, 2));
        }
    }
}
