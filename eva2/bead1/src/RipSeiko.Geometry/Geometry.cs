using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RipSeiko.Geometry
{
    public static class Geometry
    {
        public static bool RelativelyClose(float x, float y, float epsilon = 1E-10f)
        {
            if (x == y) return true;

            float absX = Math.Abs(x);
            float absY = Math.Abs(y);
            float diff = Math.Abs(x - y);

            if (x * y == 0)
                return diff < (epsilon * epsilon);
            else if (absX + absY == diff)
                return diff < epsilon;
            else
                return diff / (absX + absY) < epsilon;
        }

        public static bool Intersect(PointF p1, PointF p2) => p1 == p2;

        public static Tuple<PointF,PointF> Reorder(PointF p1, PointF p2)
        {
            return Tuple.Create(
                new PointF(Math.Min(p1.X, p2.X), Math.Min(p1.Y, p2.Y)),
                new PointF(Math.Max(p1.X, p2.X), Math.Max(p1.Y, p2.Y))
            );
        }

        public static bool Intersect(PointF p, RectangleF r) => r.InBounds(p);

        public static bool Intersect(RectangleF r, PointF p) => Intersect(p, r);

        public static bool Paralell(LineF l1, LineF l2)
            => Geometry.RelativelyClose(
                (l1.P2.Y - l1.P1.Y) * (l2.P1.X - l2.P2.X),
                (l2.P2.Y - l2.P1.Y) * (l1.P1.X - l1.P2.X)
            );

        public static IntersectionType Intersect(LineF l, PointF p)
            => l.P1 == p || l.P2 == p ?
                IntersectionType.Improper
                :
                RelativelyClose(
                    (p.Y + l.P1.Y) * (l.P2.X - l.P1.X),
                    (p.X + l.P1.X) * (l.P2.Y - l.P1.Y)
                ) ?
                    IntersectionType.Proper
                    :
                    IntersectionType.None;

        public static IntersectionType Intersect(PointF p, LineF l) => Intersect(l, p);

        private static int RelativePosition(Line l, Point p)
        {
            float x1 = p.X - l.P1.X;
            float x2 = (l.P2.Y - l.P1.Y) / (float)(l.P2.X - l.P1.X) * x1;
            return Math.Sign(x2.CompareTo(p.Y));
        }

        public static int Orientation(PointF p1, PointF p2, PointF p3)
        {
            //See http://www.geeksforgeeks.org/orientation-3-ordered-points/
            return Math.Sign((p2.Y - p1.Y) * (p3.X - p2.X) - (p3.Y - p2.Y) * (p2.X - p1.X));
        }

        public static bool StrictlyOnSegment(LineF l, PointF p)
        {
            return Intersect(l, p) == IntersectionType.Proper;
        }

        private static bool OnSegment(LineF l, PointF p)
        {
            return Intersect(l, p) != IntersectionType.None;
        }

        public enum IntersectionType { None, Proper, Improper, Infinite }

        public static IntersectionType Intersect(LineF l1, LineF l2)
        {
            if (l1 == l2)
            {
                return IntersectionType.Infinite;
            }
            PointF p1 = l1.P1;
            PointF p2 = l1.P2;
            PointF q1 = l2.P1;
            PointF q2 = l2.P2;

            int o1 = Orientation(p1, p2, q1);
            int o2 = Orientation(p1, p2, q2);
            int o3 = Orientation(q1, q2, p1);
            int o4 = Orientation(q1, q2, p2);

            bool improper = p1 == q1 || p1 == q2 || p2 == q1 || p2 == q1;

            if (o1 != o2 && o3 != o4)
            {
                return IntersectionType.Proper;
            }
            else
            {
                if (o1 == 0 && o2 == 0 && o3 == 0 && o4 == 0)
                {
                    if (
                        StrictlyOnSegment(l1, q1) ||
                        StrictlyOnSegment(l1, q1) ||
                        StrictlyOnSegment(l2, p1) || 
                        StrictlyOnSegment(l2, p2)
                    ) {
                        return IntersectionType.Infinite;
                    }
                    else if (improper)
                    {
                        if (p1 == q1 && p2 == q2 || p2 == q1 && p1 == q2)
                        {
                            return IntersectionType.Infinite;
                        }
                        else
                        {
                            return IntersectionType.Improper;
                        }
                    }
                    else
                    {
                        return IntersectionType.None;
                    }
                }
                else
                {
                    return IntersectionType.None;
                }
            }
        }

        public static bool Intersect(RectangleF r, LineF l)
        {
            if (r.InBounds(l.P1) || r.InBounds(l.P2))
            {
                return true;
            }

            LineF side1 = new LineF(r.BottomLeft, r.BottomRight);
            LineF side2 = new LineF(r.BottomRight, r.TopRight);
            LineF side3 = new LineF(r.BottomLeft, r.TopLeft);
            LineF side4 = new LineF(r.TopLeft, r.TopRight);

            var what =
                (new[] { side1, side2, side3, side4 })
                .Select(side => Tuple.Create(Intersect(side, l), side))
                .Where(t => t.Item1 == IntersectionType.Proper)
                .ToArray();
            return what.Length >= 2;
        }

        public static bool Intersect(LineF l, RectangleF r) => Intersect(r, l);
    }
}
