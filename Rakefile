task :package do |t|
        require 'rbconfig'
	require 'pathname'
        include Config

	def deploy_raw(target)
                rm_rf target
                Dir.mkdir target
                cp Pathname.getwd + "README.md", target
		bindir = Pathname.getwd + "dist" + "build" + "metal"
		if (CONFIG['host_os']=~/w32/)
			binfile=bindir+"metal.exe"
		else
			binfile=bindir+"metal"
		end
                cp binfile, File.join(target,"/metal")
        end
	def deploy(target)
		deploy_raw target
                sh "tar zcvf #{target}.tar.gz #{target}"
	end
	def deploy_7z(target)
		deploy_raw target
		sh "7z a #{target}.zip #{target}"
	end

        version = File.open("dist.cabal").grep(/^Version/)[0].split(" ")[1]
        puts CONFIG['host_os'] + " " + CONFIG['host_cpu']
        if (CONFIG['host_os']=="linux")
                if (CONFIG['host_cpu']=="amd64")
                        sh "cabal clean && cabal configure --ghc-options='-static -optl-static -optl-pthread -O2' && cabal build"
                        target = "metal-linux64-#{version}"
                        deploy(target)
               end
               if (CONFIG['host_cpu']=~/i.86/)
                       sh "cabal clean && cabal configure --ghc-options='-static -optl-static -optl-pthread -O2' && cabal build"
                        target = "metal-linux32-#{version}"
                        deploy(target)
               end
        end

        if (CONFIG['host_os']=~/darwin/)
                        sh "cabal clean && cabal configure --ghc-options='-O2' && cabal build "
                        target = "metal-mac-#{version}"
                        deploy(target)
        end
        if (CONFIG['host_os']=~/w32/)
                        sh "cabal clean"
			sh "cabal configure --ghc-option=-O2"
			sh "cabal build"
                        target = "metal-win-#{version}"
			deploy_7z(target)
	end
end
