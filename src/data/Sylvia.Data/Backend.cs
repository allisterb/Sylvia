using System;
using System.Collections.Generic;
using System.Text;

namespace Sylvia.Data
{
    public abstract class Backend : IBackend
    {
        public abstract T[] Fill<T>(T[] a, ref T value) where T : struct;
        public abstract T[] FillRef<T>(T[] a, ref T value);
    }
}
