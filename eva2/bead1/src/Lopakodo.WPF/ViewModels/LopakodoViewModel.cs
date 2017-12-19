using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace Lopakodo.WPF.ViewModels
{
    class LopakodoViewModel : ViewModelBase
    {
        const string MAPS_FOLDER = "Maps";

        Persistance.TxtFolderMapRepository mapRepo;

        object currentViewModel;


        public object CurrentViewModel
        {
            get { return currentViewModel; }
            private set
            {
                if (value != currentViewModel)
                {
                    currentViewModel = value;
                    OnPropertyChanged("CurrentViewModel");
                }
            }
        }

        public LopakodoViewModel()
        {
            CurrentViewModel = CreateMenuViewModel();
        }

        private object CreateMenuViewModel()
        {
            return new MenuViewModel(
                new RelayCommand(() => {
                    /*await new Task(() =>
                    {
                        CurrentViewModel = CreateMapSelectionModel();
                    });*/
                    CurrentViewModel = CreateMapSelectionModel();
                }),
                new RelayCommand(() => { System.Windows.Application.Current.Shutdown(); })
            );
        }

        private object CreateMapSelectionModel()
        {
            mapRepo = new Persistance.TxtFolderMapRepository(MAPS_FOLDER);
            return new MapSelectionViewModel(
                            mapRepo.Maps,
                            new RelayCommand(() => CurrentViewModel = CreateMenuViewModel()),
                            new DelegateCommand(map => { CurrentViewModel = CreateGameModel((Persistance.TxtFolderMapRepository.MapID)map); })
                        );
        }

        private object CreateGameModel(Persistance.TxtFolderMapRepository.MapID map)
        {
            return new GameViewModel(
                mapRepo.LoadMap(map),
                () => { CurrentViewModel = CreateMenuViewModel(); }
            );
        }
    }
}
