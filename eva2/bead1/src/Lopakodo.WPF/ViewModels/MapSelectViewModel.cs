using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace Lopakodo.WPF.ViewModels
{
    class MapSelectionViewModel : ViewModelBase
    {
        public IReadOnlyList<Persistance.TxtFolderMapRepository.MapID> AvailableMaps { get; private set; }
        public ICommand BackCommand { get; private set; }
        public ICommand SelectMapCommand { get; private set; }

        public MapSelectionViewModel(
            IReadOnlyList<Persistance.TxtFolderMapRepository.MapID> maps,
            ICommand backCommand,
            ICommand selectMapCommand
        )
        {
            AvailableMaps = maps;
            BackCommand = backCommand;
            SelectMapCommand = selectMapCommand;
        }
    }
}
