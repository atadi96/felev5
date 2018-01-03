using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lopakodo.Persistance
{
    public interface IMapID
    {
        string Name { get; }
        string Filename { get; }
        int Width { get; }
        int Height { get; }
    }
}
