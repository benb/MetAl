task :package do |t|
        require 'rbconfig'
        include Config

        def deploy(target)
                rm_rf target
                Dir.mkdir target
                copy_entry "dist/build/metal/metal", target+"/metal", preserve=true #doesn't screw up permisions on mac
                cp "README.md", target
                sh "tar zcvf #{target}.tar.gz #{target}"
        end

        version = `grep ^Version: dist.cabal | awk '{print $2}'`.chomp
        puts CONFIG['host_os'] + " " + CONFIG['host_cpu']
        if (CONFIG['host_os']=="linux")
                if (CONFIG['host_cpu']=="amd64")
                        sh "cabal clean && cabal configure --ghc-options='-static -optl-static -optl-pthread -O2' && cabal build"
                        target = "metal-linux64-#{version}"
                        deploy(target)
               end
        end

        if (CONFIG['host_os']=~/darwin/)
                        sh "cabal clean && cabal configure --ghc-options='-O2' && cabal build "
                        target = "metal-mac-#{version}"
                        deploy(target)
        end
end
