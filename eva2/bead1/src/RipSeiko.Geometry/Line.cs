using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RipSeiko.Geometry
{
    public struct Line
    {
        public Point P1 { get; private set; }

        public Point P2 { get; private set; }

        public Vector2 Vector => P2 - P1;

        public Line(Point p1, Point p2)
        {
            P1 = p1;
            P2 = p2;
        }

        public Line(Point p) : this(Point.Zero, p) { }

        public Line(Vector2 v) : this(new Point(v)) { }

        public Line(Point p, Vector2 v) : this(p, p + v) { }

        public Line(int x1, int y1, int x2, int y2) : this(new Point(x1, y1), new Point(x2, y1)) { }

        public static Line operator +(Line l, Vector2 v) => new Line(l.P1 + v, l.P2 + v);

        public static Line operator -(Line l, Vector2 v) => l + (-v);

    }

    public struct LineF : IEquatable<LineF>
    {
        public readonly PointF P1;

        public readonly PointF P2;

        public Vector2F Vector => P2 - P1;

        public LineF(PointF p1, PointF p2)
        {
            P1 = p1;
            P2 = p2;
        }

        public LineF(PointF p) : this(PointF.Zero, p) { }

        public LineF(Vector2F v) : this(new PointF(v)) { }

        public LineF(PointF p, Vector2F v) : this(p, p + v) { }

        public LineF(float x1, float y1, float x2, float y2) : this(new PointF(x1, y1), new PointF(x2, y1)) { }

        public static LineF operator +(LineF l, Vector2F v) => new LineF(l.P1 + v, l.P2 + v);

        public static LineF operator -(LineF l, Vector2F v) => l + (-v);

        public static bool operator==(LineF l1, LineF l2) => l1.Equals(l2);

        public static bool operator !=(LineF l1, LineF l2) => !l1.Equals(l2);

        public bool Equals(LineF other)
        {
            return
                other.P1 == P1 && other.P2 == P2
                || other.P2 == P1 && other.P1 == P2;
        }
    }
}
