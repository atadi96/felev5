using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RipSeiko.Geometry
{
    public class Rectangle
    {
        public Point BottomLeft { get; private set; }
        public Size Size { get; private set; }

        public Point BottomRight => BottomLeft + Size.HorizontalComponent;

        public Point TopLeft => BottomLeft + Size.VerticalComponent;

        public Point TopRight => BottomLeft + Size.Vector;

        public Rectangle(Point bottomLeft, Size s)
        {
            BottomLeft = bottomLeft;
            Size = s;
        }

        public Rectangle(Rectangle r) : this(r.BottomLeft, r.Size) { }

        public Rectangle(Size s) : this(Point.Zero, s) { }

        public Rectangle(Point bottomLeft, int sx, int sy) : this(bottomLeft, new Size(sx, sy)) { }

        public Rectangle(int px, int py, int sx, int sy) : this(new Point(px, py), new Size(sx, sy)) { }

        public Rectangle(Point bottomLeft, Vector2 v) : this(bottomLeft, v.X, v.Y) { }

        public Rectangle(Point p1, Point p2)
            : this(
                new Point(Math.Min(p1.X, p2.X), Math.Min(p1.Y, p2.Y)),
                new Size(Math.Abs(p1.X - p2.X), Math.Abs(p1.Y - p2.Y))
            ) { }

        public static Rectangle operator +(Rectangle r, Vector2 v) => new Rectangle(r.BottomLeft + v, r.Size);

        public static Rectangle operator -(Rectangle r, Vector2 v) => r + (-v);

        public static Rectangle operator *(Rectangle r, int l) => new Rectangle(r.BottomLeft * l, r.Size * l);

        public bool InBounds(Point p) => BottomLeft.X <= p.X && BottomLeft.Y <= p.Y && p.X <= TopRight.X && p.Y <= TopRight.Y;
    }

    public struct RectangleF
    {
        public PointF BottomLeft { get; private set; }

        public SizeF Size { get; private set; }

        public PointF BottomRight => BottomLeft + Size.HorizontalComponent;

        public PointF TopLeft => BottomLeft + Size.VerticalComponent;

        public PointF TopRight => BottomLeft + Size.Vector;

        public RectangleF(PointF p, SizeF s)
        {
            BottomLeft = p;
            Size = s;
        }

        public RectangleF(RectangleF r) : this(r.BottomLeft, r.Size) { }

        public RectangleF(SizeF s) : this(PointF.Zero, s) { }

        public RectangleF(PointF p, float sx, float sy) : this(p, new SizeF(sx, sy)) { }

        public RectangleF(float x1, float y1, float x2, float y2) : this(new PointF(x1, y1), new SizeF(x2, y2)) { }

        public RectangleF(PointF p, Vector2F v) : this(p, p + v) { }

        public RectangleF(PointF p1, PointF p2)
            : this(
                new PointF(Math.Min(p1.X, p2.X), Math.Min(p1.Y, p2.Y)),
                new SizeF(Math.Abs(p1.X - p2.X), Math.Abs(p1.Y - p2.Y))
            ) { }

        public static RectangleF operator +(RectangleF r, Vector2F v) => new RectangleF(r.BottomLeft + v, r.Size);

        public static RectangleF operator -(RectangleF r, Vector2F v) => r + (-v);

        public static RectangleF operator *(RectangleF r, float l) => new RectangleF(r.BottomLeft * l, r.Size * l);

        public bool InBounds(PointF p) => BottomLeft.X <= p.X && BottomLeft.Y <= p.Y && p.X <= TopRight.X && p.Y <= TopRight.Y;
    }
}
