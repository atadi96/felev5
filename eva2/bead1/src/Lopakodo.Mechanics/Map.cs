using System;
using System.Collections.Generic;
using System.Text;
using RipSeiko.Geometry;

namespace Lopakodo.Mechanics
{
    public enum FieldType
    {
        Ground,
        Wall
    }

    public class Map<T> where T : struct
    {
        public Point StartPosition { get; protected set; }

        public Point FinishPosition { get; protected set; }

        public Point[] EnemyStartPoints { get; protected set; }

        public Size Size { get; protected set; }

        private T[,] Fields;

        public Map(T[,] fields, Point startPosition, Point finishPosition, Point[] enemyStartPositions)
        {
            StartPosition = startPosition;
            FinishPosition = finishPosition;
            EnemyStartPoints = enemyStartPositions;
            Fields = fields;
            Size = new Size(fields.GetLength(0), fields.GetLength(1));
            Bounds = new Rectangle(Point.Zero, Size);
        }

        public Rectangle Bounds { get; protected set; }

        public bool InBounds(Point p) => Bounds.InBounds(p);

        public T? this[Point p]
        {
            get
            {
                if (InBounds(p))
                {
                    return Fields[p.X, p.Y];
                }
                else
                {
                    return null;
                }
            }
        }

        public class GameState
        {
            public Map<FieldType> Map { get; private set; }
            public Point PlayerPosition { get; private set; }
            public Point[] EnemyPositions { get; private set; }


        }

    }
}
