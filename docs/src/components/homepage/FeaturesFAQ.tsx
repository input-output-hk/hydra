import React, { FC } from "react";
import { forLaptop } from "../../../helpers/media-queries";
import useMediaQuery from "../../hooks/useMediaQuery";
import { motion } from "framer-motion";
import { FeaturesContent } from "../../../docs/homepage/features-faq";

type Props = {
  title: string;
  description: string;
};

const FAQ: FC<Props> = ({ title, description }) => {
  return (
    <motion.div
      className="flex text-white tablet:flex-row flex-col"
      initial="hidden"
      whileInView="visible"
      viewport={{ once: true }}
      transition={{ duration: 0.35, delay: 0.3 }}
      variants={{
        visible: { opacity: 1, y: 0 },
        hidden: { opacity: 0, y: 100 },
      }}
    >
      <span className="text-2xl min-w-40 tablet:self-center">{title}</span>
      <p className="tablet:border-l tablet:border-t-0 border-t border-solid border-white/25 tablet:pl-12 tablet:ml-12 mt-3 pt-3 tablet:pt-0 tablet:mt-0 max-w-xl">
        {description}
      </p>
    </motion.div>
  );
};

const FeaturesFAQ: FC = () => {
  const isLaptopUp = useMediaQuery(forLaptop);
  return (
    <motion.section
      className="bg-teal"
      initial="hidden"
      whileInView="visible"
      viewport={{ once: true }}
      transition={{ duration: 0.35, delay: 0.25 }}
      variants={{
        visible: { opacity: 1, y: 0 },
        hidden: { opacity: 0, y: 100 },
      }}
    >
      <div className="component pageContainer flex flex-col">
        <h5 className="text-base text-white">/ FEATURES</h5>
        <div className="flex flex-col laptop:flex-row justify-between">
          <div className="pt-14 laptop:pr-14 flex flex-col gap-8">
            {FeaturesContent.map((props, idx) => (
              <FAQ key={idx} {...props} />
            ))}
          </div>
          {isLaptopUp && (
            <div className="max-w-[345px] max-h-[345px] self-center">
              <video
                autoPlay
                muted
                loop
                playsInline
                preload="auto"
                onPlaying={(e) => {
                  (e.target as HTMLVideoElement).style.opacity = "1";
                }}
                aria-label="Video background"
                className="rounded-full object-contain"
              >
                <source src={"features-faq-video.mp4"} type="video/mp4" />
              </video>
            </div>
          )}
        </div>
      </div>
    </motion.section>
  );
};

export default FeaturesFAQ;
