using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Threading.Tasks;
using Lopakodo.Mechanics;

namespace Lopakodo
{
    static class Pictures
    {
        public static readonly Image Submarine = Properties.Resources.sub;
        public static readonly Image Destroyer = Properties.Resources.dd;
        public static readonly Image Radar = Properties.Resources.radar;
        public static readonly Image Menu = Properties.Resources.menu2;

        private static RotateFlipType ConvertDirection(GameState.Direction direction)
        {
            switch (direction)
            {
                case GameState.Direction.Right:
                    return RotateFlipType.Rotate90FlipNone;
                case GameState.Direction.Down:
                    return RotateFlipType.Rotate180FlipNone;
                case GameState.Direction.Left:
                    return RotateFlipType.Rotate270FlipNone;
                case GameState.Direction.None:
                case GameState.Direction.Up:
                default:
                    return RotateFlipType.RotateNoneFlipNone;
            }
        }

        public static Image Rotate(Image img, GameState.Direction direction)
        {
            Image bm = new Bitmap(img);
            bm.RotateFlip(ConvertDirection(direction));
            return bm;
        }
    }
}
