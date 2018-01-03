using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Lopakodo.Mechanics;
using System.Reflection;

namespace Lopakodo.Persistance
{
    public abstract class BaseMapRepository : IMapRepository<IMapID, FieldType>
    {
        public abstract IReadOnlyList<IMapID> Maps { get; }

        public abstract Map<FieldType> LoadMap(IMapID mapID);

        protected System.IO.Stream GetFile(string filename)
        {
            var assembly = typeof(BaseMapRepository).GetTypeInfo().Assembly;
            return assembly.GetManifestResourceStream("Lopakodo.Maps." + filename);
        }
    }
}
