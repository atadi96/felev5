using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using MemoryGame.Model;
using System.Windows.Input;
using System.Windows.Media;
using System.Collections.ObjectModel;
using System.Windows;
using System.Windows.Media.Imaging;

namespace MemoryGame.WPF.ViewModels
{
    class ViewCard
    {
        public int ID { get; set; }
        public ImageSource Image { get; set; }

        public ViewCard() { }

        public ViewCard(int id, ImageSource img)
        {
            ID = id;
            Image = img;
        }
    }

    class GameViewModel : ViewModelBase
    {
        private Game game;

        private BitmapImage cardImages;

        private ICommand newGameCommand;
        public ICommand NewGameCommand
        {
            get
            {
                if (newGameCommand == null)
                {
                    newGameCommand = new DelegateCommand(o => {
                        int size = Convert.ToInt32((string)o);
                        game = new Game(size);
                        game.TickEvent += (object o2, EventArgs e) =>
                            Application.Current.Dispatcher.Invoke(() => UpdateView());
                        UpdateView();
                    });
                }
                return newGameCommand;
            }
        }

        private ICommand clickCommand;
        public ICommand ClickCommand
        {
            get
            {
                if (clickCommand == null)
                {
                    clickCommand = new DelegateCommand(o => {
                        int id = (int)o;
                        game.ClickID(id);
                        UpdateCards();
                    });
                }
                return clickCommand;
            }
        }

        private ObservableCollection<ViewCard> cards;
        public ObservableCollection<ViewCard> Cards {
            get
            {
                return cards;
            }
            private set
            {
                if (value != cards)
                {
                    cards = value;
                    OnPropertyChanged("Cards");
                }
            }
        }

        public BitmapSource TargetImage { get; private set; }

        public GameViewModel()
        {
            game = new Game(4);
            game.TickEvent += (object o, EventArgs e) =>
                Application.Current.Dispatcher.Invoke(() => UpdateView());
            cardImages = new BitmapImage();
            cardImages.BeginInit();
            cardImages.UriSource = new Uri("Resources/cards.png", UriKind.Relative);
            cardImages.EndInit();
            UpdateView();
        }

        private void UpdateView()
        {
            TargetImage =
                new CroppedBitmap(cardImages, IdToCrop(game.TargetFieldID));
            OnPropertyChanged("TargetImage");
            UpdateCards();
        }

        private Int32Rect IdToCrop(int id)
        {
            const int width = 200;
            const int height = 274;
            return new Int32Rect(id * width, 0, width, height);
        }

        private void UpdateCards()
        {
            var newCards =
                game.Fields
                    .Select(field => field.Visible
                                ? new { id = field.ID, picID = field.ID }
                                : new { id = field.ID, picID = 16 }
                    ).Select(data => new ViewCard(
                                        data.id,
                                        new CroppedBitmap(
                                            cardImages,
                                            IdToCrop(data.picID)
                                        )
                    )).ToArray();
            Cards = new ObservableCollection<ViewCard>(newCards);
        }
    }
}
