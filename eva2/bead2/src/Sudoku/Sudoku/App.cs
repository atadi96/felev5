using System;
using System.Threading.Tasks;
using Xamarin.Forms;
using Lopakodo.View;
using Lopakodo.ViewModel;
using Lopakodo.Mechanics;
using Lopakodo.Persistance;

namespace Lopakodo
{
    public class App : Application
    {
        MapsPage _mapsPage;
        MapsViewModel _mapsViewModel;
        NavigationPage _navigation;
        IMapRepository<IMapID, FieldType> _mapRepo;
        GameView _gameView;

        bool _gameRunning = true;

        public App()
        {
            _mapRepo = DependencyService.Get<IMapRepository<IMapID, FieldType>>();
            _mapsPage = new MapsPage();
            _mapsViewModel = new MapsViewModel(_mapRepo);
            _mapsViewModel.MapSelected +=
                async (object o, IMapID mapID) =>
                {
                    var map = await Task.Run<Map<FieldType>>(() =>
                    {
                        return _mapRepo.LoadMap(mapID);
                    });
                    //var map = _mapRepo.LoadMap(mapID);
                    _gameView = new GameView();
                    var gameVM = new GameViewModel(map, mapID.Name);
                    _gameRunning = gameVM.Running;
                    gameVM.PropertyChanged += (object oo, System.ComponentModel.PropertyChangedEventArgs propCh) =>
                    {
                        if (propCh.PropertyName == "Running")
                        {
                            bool running = ((GameViewModel)oo).Running;
                            if (running && !_gameRunning)
                            {
                                _gameRunning = running;
                                Device.StartTimer(TimeSpan.FromSeconds(1), () =>
                                {
                                    //_gameView.DisplayAlert("k", "k", "k");
                                    gameVM.UpdateGame();
                                    return _gameRunning;
                                });
                            }
                            else
                            {
                                _gameRunning = running;
                            }
                        }
                    };
                    gameVM.GameEnd += async (object oo, GameState.GameStatus status) =>
                    {
                        await _gameView.DisplayAlert("Game Result", status == GameState.GameStatus.Won ? "You win!" : "You lose!", "OK");
                    };
                    _gameView.BindingContext = gameVM;
                    Device.StartTimer(TimeSpan.FromSeconds(1), () =>
                    {
                        //_gameView.DisplayAlert("k", "k", "k");
                        gameVM.UpdateGame();
                        return _gameRunning;
                    });
                    await _navigation.PushAsync(_gameView);
                };
            _mapsPage.BindingContext = _mapsViewModel;
            
            _navigation = new NavigationPage(_mapsPage);
            MainPage = _navigation;
        }

        protected override void OnStart()
        {

            //Device.StartTimer(TimeSpan.FromSeconds(1), () => { _sudokuGameModel.AdvanceTime(); return _advanceTimer; }); // elindítjuk az időzítőt
        }

        protected override void OnSleep()
        {
            //_advanceTimer = false;
        }

        protected override void OnResume()
        {
            // betöltjük a felfüggesztett játékot, amennyiben van
            /*try
            {
                Task.Run(async () =>
                {
                    await _sudokuGameModel.LoadGame("SuspendedGame");
                    _sudokuViewModel.RefreshTable();

                    // csak akkor indul az időzítő, ha sikerült betölteni a játékot
                    _advanceTimer = true;
                    Device.StartTimer(TimeSpan.FromSeconds(1), () => { _sudokuGameModel.AdvanceTime(); return _advanceTimer; });
                });
            }
            catch { }*/

        }
        private async void SudokuGameModel_GameOver(object sender, /*Sudoku*/EventArgs e)
        {
            /*_advanceTimer = false;

            if (e.IsWon) // győzelemtől függő üzenet megjelenítése
            {
                await MainPage.DisplayAlert("Sudoku játék", "Gratulálok, győztél!" + Environment.NewLine +
                                            "Összesen " + e.GameStepCount + " lépést tettél meg és " +
                                            TimeSpan.FromSeconds(e.GameTime).ToString("g") + " ideig játszottál.",
                                            "OK");
            }
            else
            {
                await MainPage.DisplayAlert("Sudoku játék", "Sajnálom, vesztettél, lejárt az idő!", "OK");
            }*/
        }
    }
}
