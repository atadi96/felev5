using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RipSeiko.Geometry
{
    public struct Vector2
    {
        public int X { get; private set; }
        public int Y { get; private set; }

        public Vector2(int x, int y)
        {
            X = x;
            Y = y;
        }

        public Vector2(int v) : this(v, v) { }

        public Vector2(Vector2 v) : this(v.X, v.Y) { }

        public Vector2(Point p) : this(p.X, p.Y) { }

        public double Length
        {
            get
            {
                return Math.Sqrt(X * X + Y * Y);
            }
        }

        public static Vector2 operator +(Vector2 a, Vector2 b) => new Vector2(a.X + b.X, a.Y + b.Y);

        public static Vector2 operator -(Vector2 a) => new Vector2(-a.X, -a.Y);

        public static Vector2 operator -(Vector2 a, Vector2 b) => a + (-b);

        public static Vector2 operator *(Vector2 a, int l) => new Vector2(a.X * l, a.Y * l);

        public static Vector2 operator /(Vector2 a, int l) => new Vector2(a.X * l, a.Y / l);

        public int Dot(Vector2 a) => X * a.X + Y * a.Y;

        public Vector2F Normalized => new Vector2F(X, Y).Normalized;

        public override string ToString() => "(" + X + "," + Y + ")";

        public override bool Equals(object obj)
        {
            if (obj == null)
            {
                return false;
            }
            if (obj.GetType() == this.GetType())
            {
                Vector2 other = (Vector2)obj;
                return X == other.X && Y == other.Y;
            }
            else
            {
                return false;
            }
        }

        public override int GetHashCode() => X.GetHashCode() ^ Y.GetHashCode();
    }

    public struct Vector2F
    {
        public float X { get; private set; }
        public float Y { get; private set; }

        public Vector2F(float x, float y)
        {
            X = x;
            Y = y;
        }

        public Vector2F(float v) : this(v, v) { }

        public Vector2F(Vector2F v) : this(v.X, v.Y) { }

        public Vector2F(PointF p) : this(p.X, p.Y) { }

        public double Length
        {
            get
            {
                return Math.Sqrt(X * X + Y * Y);
            }
        }

        public static Vector2F operator +(Vector2F a, Vector2F b) => new Vector2F(a.X + b.X, a.Y + b.Y);

        public static Vector2F operator -(Vector2F a) => new Vector2F(-a.X, -a.Y);

        public static Vector2F operator -(Vector2F a, Vector2F b) => a + (-b);

        public static Vector2F operator *(Vector2F a, float l) => new Vector2F(a.X * l, a.Y * l);

        public static Vector2F operator /(Vector2F a, float l) => a * (1 / l);

        public float Dot(Vector2F a) => X * a.X + Y * a.Y;

        public Vector2F Normalized => new Vector2F(X / (float)Length, Y / (float)Length);

        public override string ToString() => "(" + X + "," + Y + ")";

        public override bool Equals(object obj)
        {
            if (obj == null)
            {
                return false;
            }
            if (obj.GetType() == this.GetType())
            {
                Vector2F other = (Vector2F)obj;
                return X == other.X && Y == other.Y;
            }
            else
            {
                return false;
            }
        }

        public override int GetHashCode() => X.GetHashCode() ^ Y.GetHashCode();
    }
}
