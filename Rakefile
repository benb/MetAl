task :package do |t|
        require 'rbconfig'
        include Config
        version = `grep ^Version: dist.cabal | awk '{print $2}'`.chomp
        puts CONFIG['host_os'] + " " + CONFIG['host_cpu']
        if (CONFIG['host_os']=="linux")
                if (CONFIG['host_cpu']=="amd64")
                        sh "cabal clean && cabal configure --ghc-options='-static -optl-static -optl-pthread -O2' && cabal build"
                        target = "metal-linux64-#{version}"
                        rm_rf target
                        Dir.mkdir target
                        cp "dist/build/metal/metal", target
                        cp "README.md", target
                        sh "tar zcvf #{target}.tar.gz #{target}"
                end
        end
end
