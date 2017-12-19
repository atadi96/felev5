using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Timers;
using Lopakodo.Mechanics;
using Geometry = RipSeiko.Geometry;
using System.Windows.Shapes;
using System.Windows.Media;
using System.Windows;
using System.Windows.Input;

namespace Lopakodo.WPF.ViewModels
{
    class GameDrawable : DependencyObject
    {
        public int X { get; set; }
        public int Y { get; set; }
        public int Z { get; set; }
        public int Width { get; set; }
        public int Height { get; set; }
        /*
        public GameDrawable(int x, int y, int z, int width, int height)
        {
            X = x; Y = y; Z = z; Width = width; Height = height;
        } */
        public static T FromTransformData<T>(TransformData data, int zindex = 0) where T : GameDrawable, new()
        {
            return new T
            {
                X = data.X,
                Y = data.Y,
                Z = zindex,
                Width = data.Size,
                Height = data.Size
            };
        }
    }

    class MapDrawable : GameDrawable
    {
        public Brush Brush { get; set; }

        public MapDrawable SetMap(Brush brush)
        {
            Brush = brush;
            return this;
        }
    }

    class ImageDrawable : GameDrawable
    {
        public Uri Source { get; set; }
        public int Rotation { get; set; }

        public ImageDrawable SetImage(Uri source, int rotation)
        {
            Source = source;
            Rotation = rotation;
            return this;
        }
    }

    class TransformData
    {
        public int X;
        public int Y;
        public int Size;
    }

    class GameViewModel : ViewModelBase
    {
        Timer timer = new Timer();
        Action exitAction;

        GameState gameState;
        GameState.Direction selectedDirection = GameState.Direction.None;

        const double canvasWidth = 800;
        const double canvasHeight = 600;

        private ICommand playPauseCommand;
        public ICommand PlayPauseCommand
        {
            get
            {
                if (playPauseCommand == null)
                {
                    playPauseCommand = new RelayCommand(() =>
                    {
                        if (timer.Enabled)
                        {
                            timer.Stop();
                        }
                        else
                        {
                            timer.Start();
                        }
                    });
                }
                return playPauseCommand;
            }
        }

        private ICommand moveCommand;
        public ICommand MoveCommand
        {
            get
            {
                if (moveCommand == null)
                {
                    moveCommand = new DelegateCommand((object dir_) =>
                    {
                        string dir = dir_ as string;
                        if (dir == "down")
                        {
                            selectedDirection = GameState.Direction.Down;
                        }
                        else if (dir == "up")
                        {
                            selectedDirection = GameState.Direction.Up;
                        }
                        else if (dir == "right")
                        {
                            selectedDirection = GameState.Direction.Right;
                        }
                        else if (dir == "left")
                        {
                            selectedDirection = GameState.Direction.Left;
                        }
                        else
                        {
                            selectedDirection = GameState.Direction.None;
                        }
                    });
                }
                return moveCommand;
            }
        }

        private ObservableCollection<GameDrawable> gameObjects;
        public ObservableCollection<GameDrawable> GameObjects
        {
            get
            {
                return gameObjects;
            }
            private set
            {
                if (gameObjects != value)
                {
                    gameObjects = value;
                    OnPropertyChanged("GameObjects");
                }
            }
        }

        public GameViewModel(Map<FieldType> map, Action exitAction)
        {
            gameState = new GameState(map);
            this.exitAction = exitAction;
            timer.Elapsed += (object o, ElapsedEventArgs e) => { GameTick(); };
            UpdateGameObjects();
            timer.Enabled = true;
            timer.Interval = 1000;
        }

        private void GameTick()
        {
            App.Current.Dispatcher.Invoke(() =>
            {
                if (gameState.Status == GameState.GameStatus.OnGoing)
                {
                    gameState.StepState(selectedDirection);
                    selectedDirection = GameState.Direction.None;

                    UpdateGameObjects();
                }
                if (gameState.Status != GameState.GameStatus.OnGoing)
                {
                    timer.Enabled = false;
                    MessageBox.Show(gameState.Status == GameState.GameStatus.Won ? "Mission Success!" : "Mission Failed!", "End Game");
                    exitAction?.Invoke();
                }
            });
        }

        private void UpdateGameObjects()
        {
            if (double.IsNaN(canvasHeight) || double.IsNaN(canvasWidth)) return;

            ObservableCollection<GameDrawable> newDrawables = new ObservableCollection<GameDrawable>();

            Geometry.Size mapSize = gameState.Map.Size;
            float scale = GetScale(mapSize);
            var translation =
                new Geometry.Vector2F(
                    ((float)canvasWidth - mapSize.X * scale) / 2,
                    ((float)canvasHeight - mapSize.Y * scale) / 2
                );

            Geometry.Matrix3F toCenter = Geometry.Matrix3F.Translate(translation);
            Geometry.Matrix3F camera = toCenter * Geometry.Matrix3F.Scale(scale);
            Func<Geometry.Point, TransformData> transform =
                (Geometry.Point p) =>
                {
                    var v = (Geometry.Point)(Geometry.PointF)(camera * new Geometry.Vector2F(p.X, p.Y));
                    int size = (int)scale + 2;
                    return new TransformData
                    {
                        X = v.X,
                        Y = v.Y,
                        Size = size
                    };
                };
            int zindex = 0;
            for (int x = 0; x < mapSize.X; x++)
            {
                for (int y = 0; y < mapSize.Y; y++)
                {
                    FieldType? f = gameState.Map[x, y];
                    TransformData transformData = transform(new Geometry.Point(x, y));
                    Color color;
                    switch (f)
                    {
                        case FieldType.Ground:
                            color = Colors.DarkBlue;
                            break;
                        case FieldType.Wall:
                            color = Colors.Green;
                            break;
                        default:
                            color = Colors.Black;
                            break;
                    }
                    newDrawables.Add(
                        GameDrawable
                            .FromTransformData<MapDrawable>(transformData, zindex)
                            .SetMap(new SolidColorBrush(color))
                    );
                }
            }
            ++zindex;
            TransformData finishData = transform(gameState.Map.FinishPosition);
            newDrawables.Add(
                GameDrawable
                    .FromTransformData<MapDrawable>(finishData, zindex)
                    .SetMap(new SolidColorBrush(Colors.LightGreen))
            );
            ++zindex;
            foreach (var visibleField in gameState.VisibleFields)
            {
                TransformData transData = transform(visibleField);
                newDrawables.Add(
                    GameDrawable
                        .FromTransformData<ImageDrawable>(
                            transform(visibleField),
                            zindex
                        ).SetImage(
                            new Uri("Resources/radar.png", UriKind.Relative),
                            0
                        )
                );
            }
            ++zindex;
            foreach (var enemy in gameState.Enemies)
            {
                TransformData transData = transform(enemy.Position);
                newDrawables.Add(
                    GameDrawable
                        .FromTransformData<ImageDrawable>(
                            transform(enemy.Position),
                            zindex
                        ).SetImage(
                            new Uri("Resources/dd.png", UriKind.Relative),
                            RotationFrom(GameState.FromVector2(enemy.Direction))
                        )
                );
            }
            ++zindex;
            TransformData playerTrans = transform(gameState.Player.Position);
            newDrawables.Add(
                GameDrawable
                    .FromTransformData<ImageDrawable>(
                        transform(gameState.Player.Position),
                        zindex
                    ).SetImage(
                        new Uri("Resources/sub.png", UriKind.Relative),
                        RotationFrom(GameState.FromVector2(gameState.Player.Direction))
                    )
            );

            GameObjects = newDrawables;
        }

        private float GetScale(Geometry.Size mapSize)
        {
            float mapScale = (float)mapSize.X / mapSize.Y;
            float panelScale = (float)canvasWidth / (float)canvasHeight;
            if (panelScale >= mapScale) //a panel szélesebb mint a kép => a magasság lesz szűkebb
            {
                return (float)canvasHeight / mapSize.Y;
            }
            else
            {
                return (float)canvasWidth / mapSize.X;
            }
        }

        private int RotationFrom(GameState.Direction dir)
        {
            switch (dir)
            {
                case GameState.Direction.Right:
                    return 90;
                case GameState.Direction.Down:
                    return 180;
                case GameState.Direction.Left:
                    return 270;
                case GameState.Direction.Up:
                case GameState.Direction.None:
                default:
                    return 0;
            }
        }
    }
}
