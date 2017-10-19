using System;
using System.Linq;
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

        protected Point[] _enemyStartPoints;

        public IReadOnlyList<Point> EnemyStartPoints => _enemyStartPoints;

        public Size Size { get; protected set; }

        private T[,] _fields;

        public Map(T[,] fields, Point startPosition, Point finishPosition, Point[] enemyStartPositions)
        {
            StartPosition = startPosition;
            FinishPosition = finishPosition;
            _enemyStartPoints = enemyStartPositions;
            _fields = fields;
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
                    return _fields[p.X, p.Y];
                }
                else
                {
                    return null;
                }
            }
        }
    }

    public class GameState
    {
        public struct Entity
        {
            public Point Position;
            public Vector2 Direction;

            public Entity(Point pos, Vector2 dir)
            {
                Position = pos;
                Direction = dir;
            }
        }

        public enum GameStatus { OnGoing, Won, Lost }

        public enum Direction { Up, Right, Down, Left, None }

        private Entity[] _enemies;
        private Point[] _visibleFields;

        public int Updates { get; private set; }
        public Map<FieldType> Map { get; private set; }
        public GameStatus Status { get; private set; }
        public Entity Player { get; private set; }
        public IReadOnlyList<Entity> Enemies => _enemies;
        public IReadOnlyList<Point> VisibleFields => _visibleFields;

        public GameState(Map<FieldType> map)
        {
            Map = map;
            Status = GameStatus.OnGoing;
            Player = new Entity(map.StartPosition, Vector2.Zero);
            _enemies =
                Map.EnemyStartPoints
                    .Select(pos => new Entity(pos, Vector2.Zero))
                    .ToArray();
            StepState(Direction.None);
            for (int i = 0; i < _enemies.Length; i++)
            {
                _enemies[i].Direction = RandomDirection();
            }
            Updates = 0;
        }

        public static Vector2 Translate(Direction dir)
        {
            switch (dir)
            {
                case Direction.Up:
                    return new Vector2(0, 1);
                case Direction.Right:
                    return new Vector2(1, 0);
                case Direction.Down:
                    return new Vector2(0, -1);
                case Direction.Left:
                    return new Vector2(-1, 0);
                case Direction.None:
                    return Vector2.Zero;
                default:
                    throw new ArgumentException("Invalid direction");
            }
        }

        private RectangleF? Hitbox(Point p)
        {
            FieldType? field = Map[p];
            if (field == null || field.Value == FieldType.Wall)
            {
                return new RectangleF((PointF)p, new Vector2F(1, 1));
            }
            else
            {
                return null;
            }
        }

        public bool MoveEntity(ref Entity entity)
        {
            var newPos = entity.Position + entity.Direction;
            if (!Hitbox(newPos).HasValue)
            {
                entity.Position = newPos;
                return true;
            }
            else
            {
                return false;
            }
        }

        private IEnumerable<Point> VisibleFieldsFrom(Point p)
        {
            var proximity =
                (from x in new int[] { -1, 0, 1 }
                 from y in new int[] { -1, 0, 1 }
                 select
                     new {
                         Coord = p + new Vector2(x, y),
                         Center = (PointF)p + new Vector2F(x + 0.5f, y + 0.5f),
                         Hitbox = Hitbox(p + new Vector2(x, y))
                     }
                ).ToArray();
            var hitboxes =
                (from field in proximity
                 where field.Hitbox.HasValue
                 select field.Hitbox.Value)
                .ToArray();
            PointF viewPoint = (PointF)p + new Vector2F(0.5f, 0.5f);
            return
                from field in proximity
                where hitboxes.Where(hitbox => Geometry.Intersect(new LineF(viewPoint, field.Center), hitbox)).Count() == 0
                select field.Coord;
        }

        private Random random = new Random();

        private Vector2 RandomDirection()
        {
            Direction dir = (Direction)random.Next(0, 5);
            return Translate(dir);
        }

        public void StepState(Direction playerDirection)
        {
            if (Status != GameStatus.OnGoing)
            {
                return;
            }

            ++Updates;
            var player = Player;
            bool playerMoved = MoveEntity(ref player);
            Player = player;

            for(int i = 0; i < _enemies.Length; ++i)
            {
                while (!MoveEntity(ref _enemies[i]))
                {
                    _enemies[i].Direction = RandomDirection();
                }
            }
            _visibleFields =
                _enemies.Select(entity => entity.Position)
                    .SelectMany(pos => VisibleFieldsFrom(pos))
                    .Distinct()
                    .ToArray();
            if (_visibleFields.Contains(Player.Position))
            {
                Status = GameStatus.Lost;
            }
            else if (Player.Position == Map.FinishPosition)
            {
                Status = GameStatus.Won;
            }
        }

    }
}
