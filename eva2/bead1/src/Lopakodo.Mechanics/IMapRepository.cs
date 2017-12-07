using System;
using System.Collections.Generic;
using System.Text;

namespace Lopakodo.Mechanics
{
    public interface IMapRepository<MapID, FieldType> where FieldType : struct
    {
        IReadOnlyList<MapID> Maps { get; }
        Map<FieldType> LoadMap(MapID mapID);
    }
}
