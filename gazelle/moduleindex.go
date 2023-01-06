package erlang

import (
	"os"

	"gopkg.in/yaml.v2"
)

func MergeAppToModuleindex(moduleindexPath string, erlangApp *erlangApp) error {
	return MergeToModuleindex(moduleindexPath, map[string][]string{
		erlangApp.Name: erlangApp.modules(),
	})
}

func MergeToModuleindex(moduleindexPath string, entries map[string][]string) error {
	moduleindex := make(map[string][]string)
	moduleindexFile, err := os.OpenFile(moduleindexPath, os.O_RDWR|os.O_CREATE, 0755)
	if err != nil {
		return err
	}
	defer moduleindexFile.Close()

	info, err := moduleindexFile.Stat()
	if err != nil {
		return err
	}
	if info.Size() > 0 {
		decoder := yaml.NewDecoder(moduleindexFile)
		err = decoder.Decode(moduleindex)
		if err != nil {
			return err
		}
	}

	for k, v := range entries {
		moduleindex[k] = v
	}

	moduleindexFile.Seek(0, 0)
	moduleindexFile.Truncate(0)
	encoder := yaml.NewEncoder(moduleindexFile)
	defer encoder.Close()
	return encoder.Encode(moduleindex)
}

func ReadModuleindex(file string) (map[string][]string, error) {
	moduleindex := make(map[string][]string)
	reader, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer reader.Close()

	decoder := yaml.NewDecoder(reader)
	err = decoder.Decode(moduleindex)
	if err != nil {
		return nil, err
	}
	return moduleindex, nil
}
