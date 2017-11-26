using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RipSeiko.Geometry
{
    public struct Matrix3F
    {
        private struct Vector3F
        {
            public readonly float X;
            public readonly float Y;
            public readonly float Z;

            public Vector3F(float x, float y, float z)
            {
                X = x;
                Y = y;
                Z = z;
            }

            public Vector3F(float[] kek)
            {
                X = kek[0];
                Y = kek[1];
                Z = kek[2];
            }

            public float Dot(Vector3F v) => X * v.X + Y * v.Y + Z * v.Z;
        }

        private readonly float[,] Values;

        public Matrix3F(float[,] values)
        {
            if (values.GetLength(0) == 3 && values.GetLength(1) == 3)
            {
                Values = values;
            }
            else if (values.GetLength(0) == 2 && values.GetLength(1) == 2)
            {
                Values = new float[,]
                    {
                        { values[0,0], values[0,1], 0 },
                        { values[1,0], values[1,1], 0 },
                        {           0,           0, 1 }
                    };
            }
            else
            {
                throw new ArgumentException("The values must match the size of a 2x2 or a 3x3 matrix.");
            }
        }

        public Matrix3F(float x11, float x12, float x13, float x21, float x22, float x23, float x31, float x32, float x33)
            : this(
                new float[,]
                {
                    { x11, x12, x13 },
                    { x21, x22, x23 },
                    { x31, x32, x33 }
                }
            )
        { }

        public Matrix3F(float x11, float x12, float x21, float x22)
            : this(
                new float[,]
                {
                    { x11, x12, 0 },
                    { x21, x22, 0 },
                    {   0,   0, 1 }
                }
            ) { }

        public float this[int row, int col]
        {
            get
            {
                return Values[row, col];
            }
        }

        private Vector3F this[int row]
        {
            get
            {
                return new Vector3F(Values[row, 0], Values[row, 1], Values[row, 2]);
            }
        }

        private Vector3F Row(int row)
        {
            return new Vector3F(Values[row, 0], Values[row, 1], Values[row, 2]);
        }

        private Vector3F Column(int col)
        {
            return new Vector3F(Values[0, col], Values[1, col], Values[2, col]);
        }

        public static Vector2F operator *(Matrix3F m, Vector2F v)
        {
            Vector3F v3 = new Vector3F(v.X, v.Y, 1);
            Vector3F result = new Vector3F(v3.Dot(m[0]), v3.Dot(m[1]), v3.Dot(m[2]));
            return new Vector2F(result.X, result.Y);
        }

        public static RectangleF operator *(Matrix3F m, RectangleF v)
        {
            var p1 = m * (Vector2F)v.TopLeft;
            var p2 = m * (Vector2F)v.BottomRight;
            return new RectangleF((PointF)p1, (PointF)p2);
        }

        public static Matrix3F operator*(Matrix3F m, float l)
        {
            return new Matrix3F(m[0, 0] * l, m[0, 1] * l, m[1, 0] * l, m[1, 1] * l);
        }

        public static Matrix3F operator*(Matrix3F m1, Matrix3F m2)
        {
            Vector3F row0 = m1.Row(0);
            Vector3F row1 = m1.Row(1);
            Vector3F row2 = m1.Row(2);
            Vector3F col0 = m2.Column(0);
            Vector3F col1 = m2.Column(1);
            Vector3F col2 = m2.Column(2);
            float x00 = row0.Dot(col0);
            float x01 = row0.Dot(col1);
            float x02 = row0.Dot(col2);
            float x10 = row1.Dot(col0);
            float x11 = row1.Dot(col1);
            float x12 = row1.Dot(col2);
            float x20 = row2.Dot(col0);
            float x21 = row2.Dot(col1);
            float x22 = row2.Dot(col2);
            return new Matrix3F(x00, x01, x02, x10, x11, x12, x20, x21, x22);
        }

        public static Matrix3F operator*(float l, Matrix3F m) => m * l;

        public static Matrix3F Scale(Vector2F scale) => new Matrix3F(scale.X, 0, 0, scale.Y);

        public static Matrix3F Mirror(bool x, bool y) => Scale(new Vector2F(x ? -1 : 1, y ? -1 : 1));

        public static Matrix3F Scale(float f) => Scale(new Vector2F(f));

        public static Matrix3F Translate(Vector2F v) => new Matrix3F(1, 0, v.X, 0, 1, v.Y, 0, 0, 1);

        public static Matrix3F Rotate(float radAngle) =>
            new Matrix3F((float)Math.Cos(radAngle), (float)Math.Sin(radAngle), -(float)Math.Sin(radAngle), (float)Math.Cos(radAngle));

        public static Matrix3F Zero => new Matrix3F(0, 0, 0, 0, 0, 0, 0, 0, 0);

        public static Matrix3F Identity => new Matrix3F(1, 0, 0, 1);

        public static Matrix3F Camera(Vector2F cameraPos, Vector2F screenPos, float scale, float rotationAngle)
        {
            return Mirror(false, true) * Rotate(rotationAngle) * Translate(cameraPos) * Scale(scale) * Translate(screenPos);
        }
    }
}
