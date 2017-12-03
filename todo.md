# Todo Now

* Get vec font working

## Ideas
* Server / client coms
    * File / data changes
    * Live shader reoloading
* Server side processing
    * Client asks for a mesh
    * Handler generates it server side and sends
    * Merges shaders into a shader spec
    * Annotates to allow proper error messaging
    * Also do #includes? Get rid of common.glsl hacks
    * Generate binary chunks for 3d data server side?
* Obj format loader
    * Allows me access to a shit load of content
    * Looks easy enough
    * Offline (clojure side) 
    * Server boots and converts files into resources dir
        * Only does it if needed
    * Game loads

## Binary format

### Vertbuffer Data
* Attributes
    * names, types, offsets, gl-type, num of elements

* info
    * Name
    * draw type
    * vert size

* Verts
    * Binary data 
        * vert def sz string
        * u32 data start
        * u32 num of verts
        * u32 num of indicies
        * u32 indicies[num of indices]
        * u32 veryt[num of verts]
        * data
    * Endianess?

### File format
* Attributes and info in edn file
* Binary data


