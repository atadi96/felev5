using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RipSeiko.Geometry
{
    public struct Size
    {
        public int X { get; private set; }
        public int Y { get; private set; }

        public Size(int x, int y)
        {
            if (x < 0)
            {
                throw new ArgumentException("Neither component of a size can be negative", "x");
            }
            if (y < 0)
            {
                throw new ArgumentException("Neither component of a size can be negative", "y");
            }
            X = x;
            Y = y;
        }

        public Size(int v) : this(v, v) { }

        public Size(Vector2 v) : this(v.X, v.Y) { }

        public static Size operator *(Size s, int l) => new Size(s.X * l, s.Y * l);

        public static Size operator /(Size s, int l) => new Size(s.X / l, s.Y / l);

        public Vector2 HorizontalComponent => new Vector2(X, 0);

        public Vector2 VerticalComponent => new Vector2(0, Y);

        public Vector2 Vector => new Vector2(X, Y);
    }

    public struct SizeF
    {
        public float X { get; private set; }
        public float Y { get; private set; }

        public SizeF(float x, float y)
        {
            if (x < 0)
            {
                throw new ArgumentException("Neither component of a size can be negative", "x");
            }
            if (y < 0)
            {
                throw new ArgumentException("Neither component of a size can be negative", "y");
            }
            X = x;
            Y = y;
        }

        public SizeF(float v) : this(v, v) { }

        public SizeF(Vector2F v) : this(v.X, v.Y) { }

        public static SizeF operator *(SizeF s, float l) => new SizeF(s.X * l, s.Y * l);

        public static SizeF operator /(SizeF s, float l) => s * (1 / l);

        public Vector2F HorizontalComponent => new Vector2F(X, 0);

        public Vector2F VerticalComponent => new Vector2F(0, Y);

        public Vector2F Vector => new Vector2F(X, Y);
    }


}
