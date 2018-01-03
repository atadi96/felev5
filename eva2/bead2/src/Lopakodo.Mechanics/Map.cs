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
                if (0 <= p.X && 0 <= p.Y && p.X < Size.X && p.Y < Size.Y)
                {
                    return _fields[p.X, p.Y];
                }
                else
                {
                    return null;
                }
            }
        }

        public T? this[int x, int y] => this[new Point(x,y)];
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

        public enum Direction { Up = 0, Right = 1, Down = 2, Left = 3, None = 4 }

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
                    return new Vector2(0, -1);
                case Direction.Right:
                    return new Vector2(1, 0);
                case Direction.Down:
                    return new Vector2(0, 1);
                case Direction.Left:
                    return new Vector2(-1, 0);
                case Direction.None:
                    return Vector2.Zero;
                default:
                    throw new ArgumentException("Invalid direction");
            }
        }

        public static Direction FromVector2(Vector2 v)
        {
            if (v.X == 0)
            {
                if (v.Y == 1)
                {
                    return Direction.Down;
                }
                if(v.Y == -1)
                {
                    return Direction.Up;
                }
            }
            if (v.Y == 0)
            {
                if (v.X == 1)
                {
                    return Direction.Right;
                }
                if (v.X == -1)
                {
                    return Direction.Left;
                }
            }
            return Direction.None;
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

        public bool TryMoveEntity(ref Entity entity)
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
                (from x in new int[] { -2, -1, 0, 1, 2 }
                 from y in new int[] { -2, -1, 0, 1, 2 }
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
            /*Func<PointF, RectangleF, bool> kek =
                (PointF center, RectangleF hitbox) =>
                {
                    LineF line = new LineF(viewPoint, center);
                    Geometry.Intersect(new LineF(viewPoint, center), hitbox)).ToArray();
                    return Geometry.Intersect()
                }*/
            return
                from field in proximity
                where hitboxes.Where(hitbox => Geometry.Intersect(new LineF(viewPoint, field.Center), hitbox)).Count() == 0
                select field.Coord;
        }

        private Random random = new Random();

        private Vector2 RandomDirection()
        {
            Direction dir = (Direction)random.Next(0, 5);
            while (dir == Direction.None)
            {
                dir = (Direction)random.Next(0, 5);
            }
            return Translate(dir);
        }

        private void DefinitelyMoveEntitySomewhereOrNot(ref Entity entity)
        {
            if (!TryMoveEntity(ref entity))
            {
                Entity entity2 = entity;
                var fourDirections =
                    from x in new int[] { -1, 0, 1 }
                    from y in new int[] { -1, 0, 1 }
                    where x * x + y * y == 1
                    select entity2.Position + new Vector2(x, y);
                var possibleSeq =
                    from field in fourDirections
                    where !Hitbox(field).HasValue
                    select field - entity2.Position;
                var possible = possibleSeq.ToArray();
                if (possible.Length == 0)
                {
                    entity.Direction = new Vector2(0, 0);
                }
                else
                {
                    entity.Direction = possible[random.Next(0, possible.Length)];
                    TryMoveEntity(ref entity);
                }
            }
        }

        public void StepState(Direction playerDirection)
        {
            if (Status != GameStatus.OnGoing)
            {
                return;
            }

            ++Updates;
            var player = Player;
            player.Direction = Translate(playerDirection);
            bool playerMoved = TryMoveEntity(ref player);
            Player = player;

            for(int i = 0; i < _enemies.Length; ++i)
            {
                DefinitelyMoveEntitySomewhereOrNot(ref _enemies[i]);
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
