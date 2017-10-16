using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RipSeiko.Geometry
{
    public struct Point
    {
        public readonly int X;
        public readonly int Y;

        public Point(int x, int y)
        {
            X = x;
            Y = y;
        }

        public Point(int v) : this(v, v) { }

        public Point(Point p) : this(p.X, p.Y) { }

        public Point(Vector2 v) : this(v.X, v.Y) { }

        public static bool operator ==(Point p1, Point p2) => p1.X == p2.X && p1.Y == p2.Y;

        public static bool operator !=(Point p1, Point p2) => !(p1 == p2);

        public static Point operator +(Point p, Vector2 v) => new Point(p.X + v.X, p.Y + v.Y);

        public static Point operator -(Point p) => new Point(-p.X, -p.Y);

        public static Point operator -(Point p, Vector2 v) => p + (-v);

        public static Vector2 operator -(Point a, Point b) => new Vector2(a.X - b.X, a.Y - b.Y);

        public static Point operator *(Point p, int l) => new Point(p.X * l, p.Y * l);

        public static Point operator /(Point p, int l) => new Point(p.X / l, p.Y / l);

        public static Point Zero => new Point(0, 0);

        public override string ToString() => "(" + X + "," + Y + ")";

        public override bool Equals(object obj)
        {
            if (obj == null)
            {
                return false;
            }
            if (obj.GetType() == this.GetType())
            {
                Point other = (Point)obj;
                return X == other.X && Y == other.Y;
            }
            else
            {
                return false;
            }
        }

        public override int GetHashCode() => X.GetHashCode() ^ Y.GetHashCode();
    }

    public struct PointF : IEquatable<PointF>
    {
        public readonly float X;
        public readonly float Y;

        public PointF(float x, float y)
        {
            X = x;
            Y = y;
        }

        public PointF(float v) : this(v, v) { }

        public PointF(PointF p) : this(p.X, p.Y) { }

        public PointF(Vector2F v) : this(v.X, v.Y) { }

        public static PointF operator +(PointF p, Vector2F v) => new PointF(p.X + v.X, p.Y + v.Y);

        public static PointF operator -(PointF p) => new PointF(-p.X, -p.Y);

        public static PointF operator -(PointF p, Vector2F v) => p + (-v);

        public static Vector2F operator -(PointF a, PointF b) => new Vector2F(a.X - b.X, a.Y - b.Y);

        public static PointF operator *(PointF p, float l) => new PointF(p.X * l, p.Y * l);

        public static PointF operator /(PointF p, float l) => p * (1 / l);

        public static bool operator ==(PointF p, PointF q) => p.Equals(q);

        public static bool operator !=(PointF p, PointF q) => !p.Equals(q);

        public static PointF Zero => new PointF(0, 0);

        public override string ToString() => "(" + X + "," + Y + ")";

        public override bool Equals(object obj)
        {
            if (obj == null)
            {
                return false;
            }
            if (obj.GetType() == this.GetType())
            {
                return Equals((PointF)obj);
            }
            else
            {
                return false;
            }
        }

        public override int GetHashCode() => X.GetHashCode() ^ Y.GetHashCode();

        public bool Equals(PointF other)
        {
            return
                Geometry.RelativelyClose(X, other.X)
                && Geometry.RelativelyClose(Y, other.Y);
        }
    }
}
