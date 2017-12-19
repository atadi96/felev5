using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;

namespace Lopakodo.WPF.ViewModels
{
    public class MenuViewModel : ViewModelBase
    {
        public ICommand NewGameCommand { get; private set; }
        public ICommand ExitCommand { get; private set; }

        public MenuViewModel(ICommand newGame, ICommand exit)
        {
            NewGameCommand = newGame;
            ExitCommand = exit;
        }
    }
}
