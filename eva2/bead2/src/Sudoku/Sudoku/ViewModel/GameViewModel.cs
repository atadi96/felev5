using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using Xamarin.Forms;
using Lopakodo.Mechanics;

namespace Lopakodo.ViewModel
{
    class GameViewModel : ViewModelBase
    {
        GameState gameState;
        GameState.Direction selectedDirection = GameState.Direction.None;

        public EventHandler<GameState.GameStatus> GameEnd;
        //public EventHandler<bool> RunningChanged;
        public string MapTitle { get; private set; }

        private ObservableCollection<Color> _fields;
        public ObservableCollection<Color> Fields
        {
            get { return _fields; }
            set
            {
                if (_fields != value)
                {
                    _fields = value;
                    OnPropertyChanged("Fields");
                }
            }
        }

        private bool _running;
        public bool Running
        {
            get { return _running; }
            private set
            {
                if (gameState.Status == GameState.GameStatus.OnGoing || !value)
                {
                    if (_running != value)
                    {
                        _running = value;
                        OnPropertyChanged("Running");
                        //RunningChanged(this, Running);
                    }
                }
            }
        }

        private int _mapWidth;
        public int MapWidth
        {
            get { return _mapWidth; }
            set { _mapWidth = value; OnPropertyChanged("MapSize"); }
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

        private ICommand pauseCommand;
        public ICommand PauseCommand
        {
            get
            {
                if (pauseCommand == null)
                {
                    pauseCommand = new DelegateCommand((object nothing) =>
                        { Running = !Running; }
                    );
                }
                return pauseCommand;
            }
        }

        public GameViewModel(Map<FieldType> map, string mapTitle = "")
        {
            MapTitle = mapTitle;
            MapWidth = map.Size.Y;
            gameState = new GameState(map);
            Fields = fieldsOf(gameState);
            Running = true;
        }

        private ObservableCollection<Color> fieldsOf(GameState state)
        {
            List<Color> result = new List<Color>();
            for (int y = 0; y < state.Map.Size.Y; y++)
            {
                for (int x = 0; x < state.Map.Size.X; x++)
                {
                    Color color;
                    RipSeiko.Geometry.Point pos = new RipSeiko.Geometry.Point(x, y);
                    if (state.Player.Position == pos)
                    {
                        color = Color.Blue;
                    }
                    else if (state.Map.FinishPosition == pos)
                    {
                        color = Color.Green;
                    }
                    else if (state.Enemies.Select(enemy => enemy.Position).Contains(pos))
                    {
                        color = Color.Red;
                    }
                    else if (state.VisibleFields.Contains(pos))
                    {
                        color = Color.Yellow;
                    }
                    else if (state.Map[pos] == FieldType.Ground)
                    {
                        color = Color.SaddleBrown;
                    }
                    else
                    {
                        color = Color.DarkRed;
                    }
                    result.Add(color);
                }
            }
            return new ObservableCollection<Color>(result);
        }

        public void UpdateGame()
        {
            if (gameState.Status == GameState.GameStatus.OnGoing || Running)
            {
                gameState.StepState(selectedDirection);
                Fields = fieldsOf(gameState);

                selectedDirection = GameState.Direction.None;
            }
            if (gameState.Status != GameState.GameStatus.OnGoing)
            {
                if (Running)
                {
                    GameEnd?.Invoke(this, gameState.Status);
                    Running = false;
                }
            }
        }
    }
}
